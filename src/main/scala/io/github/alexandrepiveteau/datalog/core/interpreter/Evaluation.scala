package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.*
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.*
import io.github.alexandrepiveteau.datalog.core.rule.*

import scala.collection.{immutable, mutable}

// TODO : Document this.
private def variableIndices[T](rule: List[Term[T]]): Set[List[Int]] =
  val variables = mutable.Map[Variable, mutable.ListBuffer[Int]]()
  rule.zipWithIndex.foreach { (term, index) =>
    term match {
      case variable: Variable => variables.getOrElseUpdate(variable, mutable.ListBuffer()) += index
      case Value(_) => ()
    }
  }
  variables.values.map(_.toList).toSet

// TODO : Document this.
private def constantIndices[T](rule: List[Term[T]]): List[(Int, Value[T])] =
  rule.zipWithIndex
    .filter((term, _) => term.isInstanceOf[Value[T]])
    .map((term, index) => (index, term.asInstanceOf[Value[T]]))

// TODO : Document this.
private def projection[T](predicate: List[Term[T]], rule: List[Term[T]]): List[Column[T]] =
  predicate.map {
    case term: Value[T] => Constant(term)
    case term: Variable => Index(rule.indexOf(term)) // TODO : Check because it's indexOfFirst in Kotlin
  }

// TODO : Document this.
private def selection[T](rule: List[Term[T]]): Set[Set[Column[T]]] =
  val result = mutable.Set[Set[Column[T]]]()
  val variables = variableIndices(rule)
  variables.foreach { variable => result.add(variable.map(Index.apply).toSet) }
  val constants = constantIndices(rule)
  constants.foreach { (index, value) =>
    result.add(Set(Constant(value), Index(index)))
  }
  result.toSet

// TODO : Document this.
private def evalRule[O[_, _], R[_], T: Context](rule: Rule[T], relations: List[O[T, R[T]]])
                                               (using op: IROp[O, R]): O[T, R[T]] =
  rule match
    case rule: CombinationRule[T] => evalCombinationRule(rule, relations)
    case rule: AggregationRule[T] => evalAggregationRule(rule, relations.head)

// TODO : Document this.
private def evalCombinationRule[O[_, _], R[_], T: Context](rule: CombinationRule[T], relations: List[O[T, R[T]]])
                                                          (using op: IROp[O, R]): O[T, R[T]] =
  if rule.body.size != relations.size then throw IllegalArgumentException("Invalid number of relations.")
  // 1. Negate all the relations that are negated in the rule.
  // 2. Generate a concatenation of all atoms in the rule, after the join.
  // 3. Join all the relations.
  // 4. Select the rows that match the constants and variables.
  // 5. Project the rows to the correct indices, and add constants to the projection.
  val list = relations.zipWithIndex
    .map((r, idx) => if rule.body(idx).negated then negated(rule.body(idx).arity, r) else r)
  val concat = rule.body.flatMap(_.terms.toList)
  op.join(list)
    .select(selection(concat))
    .project(projection(rule.head.terms, concat))

// TODO : Document this.
private def evalAggregationRule[O[_, _], R[_], T](rule: AggregationRule[T], relation: O[T, R[T]])
                                                 (using op: IROp[O, R], context: Context[T]): O[T, R[T]] =
  // 1. Negate the relation if the rule is negated.
  // 2. Perform the aggregation.
  val rel = if rule.clause.negated then negated(rule.clause.arity, relation) else relation
  val projection = rule.head.terms.map {
    case term: Value[T] => AggregationColumnColumn(Constant(term))
    case term: Variable =>
      if term == rule.aggregate.result then AggregationColumnAggregate
      else AggregationColumnColumn(Index(rule.clause.terms.indexOf(term)))
  }
  val same = rule.aggregate.same.toSet.map(it => Index(rule.clause.terms.indexOf(it)))
  val indices = rule.aggregate.columns.toSet.map(it => Index(rule.clause.terms.indexOf(it)))
  rel.aggregate(projection, same, rule.aggregate.agg, indices)(using context.domain)

// TODO : Document this.
private def evalRuleIncremental[O[_, _], R[_], T: Context](rule: Rule[T],
                                                           relations: List[O[T, R[T]]],
                                                           incremental: List[O[T, R[T]]])
                                                          (using op: IROp[O, R]): O[T, R[T]] =
  if rule.body.size != relations.size then throw IllegalArgumentException("Invalid number of relations.")
  if rule.body.size != incremental.size then throw IllegalArgumentException("Invalid number of relations.")
  var result = op.empty[T](rule.head.arity)
  for i <- rule.body.indices do {
    val args = List.range(0, rule.body.size).map { it => if (it == i) incremental(it) else relations(it) }
    result = result.union(evalRule(rule, args))
  }
  result.distinct()

// TODO : Document this.
private def eval[O[_, _], R[_], T: Context](predicate: PredicateWithArity,
                                            idb: RulesDatabase[T],
                                            base: Database,
                                            derived: Database)
                                           (using op: IROp[O, R]): O[T, R[T]] =
  val rules = idb(predicate)
  var result = op.empty[T](predicate.arity)
  for rule <- rules do {
    val list = rule.body.map { it =>
      val baseOp = op.scan[T](base, PredicateWithArity(it.predicate, it.arity))
      val derivedOp = op.scan[T](derived, PredicateWithArity(it.predicate, it.arity))
      baseOp.union(derivedOp)
    }
    result = result.union(evalRule(rule, list))
  }
  result.distinct()

// TODO : Document this.
private def evalIncremental[O[_, _], R[_], T: Context](predicate: PredicateWithArity,
                                                       idb: RulesDatabase[T],
                                                       base: Database,
                                                       derived: Database,
                                                       delta: Database)
                                                      (using op: IROp[O, R]): O[T, R[T]] =
  val rules = idb(predicate)
  var result = op.empty[T](predicate.arity)
  for rule <- rules do {
    val baseList = rule.body.map { it =>
      val baseOp = op.scan[T](base, PredicateWithArity(it.predicate, it.arity))
      val derivedOp = op.scan[T](derived, PredicateWithArity(it.predicate, it.arity))
      baseOp.union(derivedOp)
    }
    val deltaList = rule.body.map { it =>
      // Negation needs base facts to be present in the delta.
      val baseOp = op.scan[T](base, PredicateWithArity(it.predicate, it.arity))
      val deltaOp = op.scan[T](delta, PredicateWithArity(it.predicate, it.arity))
      baseOp.union(deltaOp)
    }
    result = result.union(evalRuleIncremental(rule, baseList, deltaList))
  }
  result.distinct()

// TODO : Document this.
def naiveEval[O[_, _], R[_], T: Context](idb: RulesDatabase[T],
                                         base: Database,
                                         result: Database)
                                        (using op: IROp[O, R]): O[T, Unit] =
  val copy = Database("Copy")
  val sequence = mutable.ListBuffer[O[T, Unit]]()
  idb.iterator.foreach { predicate =>
    sequence += op.store(copy, predicate, op.scan[T](result, predicate))
  }
  idb.iterator.foreach { predicate =>
    val res = eval(predicate, idb, base, result)
    sequence += op.store(result, predicate, res)
  }
  op.doWhileNotEqual(
    op.sequence(sequence.toList),
    result,
    copy,
  )

// TODO : Document this.
def semiNaiveEval[O[_, _], R[_], T: Context](idb: RulesDatabase[T], base: Database, result: Database)
                                            (using op: IROp[O, R]): O[T, Unit] =
  val delta = Database("Delta")
  val copy = Database("Copy")
  val outer = immutable.List.newBuilder[O[T, Unit]]
  val inner = immutable.List.newBuilder[O[T, Unit]]

  idb.iterator.foreach { predicate =>
    val res = eval(predicate, idb, base, Database.Empty)
    outer += op.store(result, predicate, res)
    outer += op.store(delta, predicate, res)
  }

  idb.iterator.foreach { p => inner += op.store(copy, p, op.scan(delta, p)) }
  idb.iterator.foreach { p =>
    val facts = evalIncremental(p, idb, base, result, copy)
    val existing = op.scan[T](result, p)
    inner += op.store(delta, p, facts.minus(existing))
  }
  idb.iterator.foreach { p =>
    val current = op.scan[T](result, p)
    val newFacts = op.scan[T](delta, p)
    inner += op.store(result, p, current.union(newFacts))
  }

  outer += op.doWhileNonEmpty(
    op.sequence(inner.result()),
    delta
  )

  op.sequence(outer.result())
