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
private def evalRule[C: Context, O[_], R[_]](rule: Rule[C], relations: List[O[R[C]]])
                                            (using op: IROp[C, O, R]): O[R[C]] =
  rule match
    case rule: CombinationRule[C] => evalCombinationRule(rule, relations)
    case rule: AggregationRule[C] => evalAggregationRule(rule, relations.head)

// TODO : Document this.
private def evalCombinationRule[C: Context, O[_], R[_]](rule: CombinationRule[C], relations: List[O[R[C]]])
                                                       (using op: IROp[C, O, R]): O[R[C]] =
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
private def evalAggregationRule[C, O[_], R[_]](rule: AggregationRule[C], relation: O[R[C]])
                                              (using op: IROp[C, O, R], context: Context[C]): O[R[C]] =
  // 1. Negate the relation if the rule is negated.
  // 2. Perform the aggregation.
  val rel = if rule.clause.negated then negated(rule.clause.arity, relation) else relation
  val projection = rule.head.terms.map {
    case term: Value[C] => AggregationColumnColumn(Constant(term))
    case term: Variable =>
      if term == rule.aggregate.result then AggregationColumnAggregate
      else AggregationColumnColumn(Index(rule.clause.terms.indexOf(term)))
  }
  val same = rule.aggregate.same.toSet.map(it => Index(rule.clause.terms.indexOf(it)))
  val indices = rule.aggregate.columns.toSet.map(it => Index(rule.clause.terms.indexOf(it)))
  rel.aggregate(projection, same, rule.aggregate.agg, indices)(using context.domain)

// TODO : Document this.
private def evalRuleIncremental[C: Context, O[_], R[_]](rule: Rule[C],
                                                        relations: List[O[R[C]]],
                                                        incremental: List[O[R[C]]])
                                                       (using op: IROp[C, O, R]): O[R[C]] =
  if rule.body.size != relations.size then throw IllegalArgumentException("Invalid number of relations.")
  if rule.body.size != incremental.size then throw IllegalArgumentException("Invalid number of relations.")
  var result = op.empty(rule.head.arity)
  for i <- rule.body.indices do {
    val args = List.range(0, rule.body.size).map { it => if (it == i) incremental(it) else relations(it) }
    result = op.union(Set(result, evalRule(rule, args)))
  }
  result.distinct()

// TODO : Document this.
private def eval[C: Context, O[_], R[_]](predicate: PredicateWithArity,
                                         idb: RulesDatabase[C],
                                         base: Database,
                                         derived: Database)
                                        (using op: IROp[C, O, R]): O[R[C]] =
  val rules = idb(predicate)
  var result = op.empty(predicate.arity)
  for rule <- rules do {
    val list = rule.body.map { it =>
      val baseOp = op.scan(base, PredicateWithArity(it.predicate, it.arity))
      val derivedOp = op.scan(derived, PredicateWithArity(it.predicate, it.arity))
      op.union(Set(baseOp, derivedOp))
    }
    result = op.union(Set(result, evalRule(rule, list)))
  }
  result.distinct()

// TODO : Document this.
private def evalIncremental[C: Context, O[_], R[_]](predicate: PredicateWithArity,
                                                    idb: RulesDatabase[C],
                                                    base: Database,
                                                    derived: Database,
                                                    delta: Database)
                                                   (using op: IROp[C, O, R]): O[R[C]] =
  val rules = idb(predicate)
  var result = op.empty(predicate.arity)
  for rule <- rules do {
    val baseList = rule.body.map { it =>
      val baseOp = op.scan(base, PredicateWithArity(it.predicate, it.arity))
      val derivedOp = op.scan(derived, PredicateWithArity(it.predicate, it.arity))
      op.union(Set(baseOp, derivedOp))
    }
    val deltaList = rule.body.map { it =>
      // Negation needs base facts to be present in the delta.
      val baseOp = op.scan(base, PredicateWithArity(it.predicate, it.arity))
      val deltaOp = op.scan(delta, PredicateWithArity(it.predicate, it.arity))
      op.union(Set(baseOp, deltaOp))
    }
    result = op.union(Set(result, evalRuleIncremental(rule, baseList, deltaList)))
  }
  result.distinct()

// TODO : Document this.
def naiveEval[C: Context, O[_], R[_]](idb: RulesDatabase[C],
                                      base: Database,
                                      result: Database)
                                     (using op: IROp[C, O, R]): O[Unit] =
  val copy = Database("Copy")
  val sequence = mutable.ListBuffer[O[Unit]]()
  idb.iterator.foreach { predicate =>
    sequence += op.store(copy, predicate, op.scan(result, predicate))
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
def semiNaiveEval[C: Context, O[_], R[_]](idb: RulesDatabase[C], base: Database, result: Database)
                                         (using op: IROp[C, O, R]): O[Unit] =
  val delta = Database("Delta")
  val copy = Database("Copy")
  val outer = immutable.List.newBuilder[O[Unit]]
  val inner = immutable.List.newBuilder[O[Unit]]

  idb.iterator.foreach { predicate =>
    val res = eval(predicate, idb, base, Database.Empty)
    outer += op.store(result, predicate, res)
    outer += op.store(delta, predicate, res)
  }

  idb.iterator.foreach { p => inner += op.store(copy, p, op.scan(delta, p)) }
  idb.iterator.foreach { p =>
    val facts = evalIncremental(p, idb, base, result, copy)
    val existing = op.scan(result, p)
    inner += op.store(delta, p, facts.minus(existing))
  }
  idb.iterator.foreach { p =>
    val current = op.scan(result, p)
    val newFacts = op.scan(delta, p)
    inner += op.store(result, p, op.union(Set(current, newFacts)))
  }

  outer += op.doWhileNonEmpty(
    op.sequence(inner.result()),
    delta
  )

  op.sequence(outer.result())
