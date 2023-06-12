package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.*
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.*
import io.github.alexandrepiveteau.datalog.core.rule.*

import scala.collection.{immutable, mutable}

// TODO : Document this.
private def variableIndices[T](rule: List[Atom[T]]): Set[List[Int]] =
  val variables = mutable.Map[Variable, mutable.ListBuffer[Int]]()
  rule.zipWithIndex.foreach { (atom, index) =>
    atom match {
      case variable: Variable => variables.getOrElseUpdate(variable, mutable.ListBuffer()) += index
      case Value(_) => ()
    }
  }
  variables.values.map(_.toList).toSet

// TODO : Document this.
private def constantIndices[T](rule: List[Atom[T]]): List[(Int, Value[T])] =
  rule.zipWithIndex
    .filter((atom, _) => atom.isInstanceOf[Value[T]])
    .map((atom, index) => (index, atom.asInstanceOf[Value[T]]))

// TODO : Document this.
private def projection[T](predicate: List[Atom[T]], rule: List[Atom[T]]): List[Column[T]] =
  predicate.map {
    case atom: Value[T] => Constant(atom)
    case atom: Variable => Index(rule.indexOf(atom)) // TODO : Check because it's indexOfFirst in Kotlin
  }

// TODO : Document this.
private def selection[T](rule: List[Atom[T]]): Set[Set[Column[T]]] =
  val result = mutable.Set[Set[Column[T]]]()
  val variables = variableIndices(rule)
  variables.foreach { variable => result.add(variable.map(Index.apply).toSet) }
  val constants = constantIndices(rule)
  constants.foreach { (index, value) =>
    result.add(Set(Constant(value), Index(index)))
  }
  result.toSet

// TODO : Document this.
private def evalRule[R[_] : Relation, T: Context](rule: Rule[T], relations: List[R[T]]): R[T] =
  rule match
    case rule: CombinationRule[T] => evalCombinationRule(rule, relations)
    case rule: AggregationRule[T] => evalAggregationRule(rule, relations.head)

// TODO : Document this.
private def evalCombinationRule[R[_] : Relation, T: Context](rule: CombinationRule[T], relations: List[R[T]]): R[T] =
  if rule.body.size != relations.size then throw IllegalArgumentException("Invalid number of relations.")
  // 1. Negate all the relations that are negated in the rule.
  // 2. Generate a concatenation of all atoms in the rule, after the join.
  // 3. Join all the relations.
  // 4. Select the rows that match the constants and variables.
  // 5. Project the rows to the correct indices, and add constants to the projection.
  val list = relations.zipWithIndex
    .map((r, idx) => if rule.body(idx).negated then negated(r) else r)
  val concat = rule.body.flatMap(_.atoms.toList)
  summon[Relation[R]].join[T](list)
    .select(selection(concat))
    .project(projection(rule.head.atoms, concat))

// TODO : Document this.
private def evalAggregationRule[R[_] : Relation, T: Context](rule: AggregationRule[T], relation: R[T]): R[T] =
  // 1. Negate the relation if the rule is negated.
  // 2. Perform the aggregation.
  val rel = if rule.clause.negated then negated(relation) else relation
  val projection = rule.head.atoms.map {
    case atom: Value[T] => AggregationColumnColumn(Constant(atom))
    case atom: Variable =>
      if atom == rule.aggregate.result then AggregationColumnAggregate
      else AggregationColumnColumn(Index(rule.clause.atoms.indexOf(atom)))
  }
  val same = rule.aggregate.same.toSet.map(it => Index(rule.clause.atoms.indexOf(it)))
  val indices = rule.aggregate.columns.toSet.map(it => Index(rule.clause.atoms.indexOf(it)))
  rel.aggregate(projection, same, rule.aggregate.agg, indices)(using summon[Context[T]].domain)

// TODO : Document this.
private def evalRuleIncremental[R[_] : Relation, T: Context](rule: Rule[T],
                                                             relations: List[R[T]],
                                                             incremental: List[R[T]]): R[T] =
  if rule.body.size != relations.size then throw IllegalArgumentException("Invalid number of relations.")
  if rule.body.size != incremental.size then throw IllegalArgumentException("Invalid number of relations.")
  var result = summon[Relation[R]].empty[T](rule.head.arity)
  for i <- rule.body.indices do {
    val args = List.range(0, rule.body.size).map { it => if (it == i) incremental(it) else relations(it) }
    result = result.union(evalRule(rule, args))
  }
  result.distinct()

// TODO : Document this.
private def eval[O[_, _], R[_] : Relation, T: Context](predicate: PredicateWithArity,
                                                       idb: RulesDatabase[T],
                                                       base: Database,
                                                       derived: Database)
                                                      (using op: IROp[O, R]): O[T, R[T]] =
  val rules = idb(predicate)
  var result = op.lift[T, R[T]](summon[Relation[R]].empty[T](predicate.arity))
  for rule <- rules do {
    val list = rule.body.map { it =>
      for
        base <- op.scan[T](base, PredicateWithArity(it.predicate, it.arity))
        derived <- op.scan[T](derived, PredicateWithArity(it.predicate, it.arity))
      yield base.union(derived)
    }
    result = for
      r <- result
      ops <- list.toOp
      res = evalRule(rule, ops)
    yield r.union(res)
  }
  result.map(_.distinct())

// TODO : Document this.
private def evalIncremental[O[_, _], R[_] : Relation, T: Context](predicate: PredicateWithArity,
                                                                  idb: RulesDatabase[T],
                                                                  base: Database,
                                                                  derived: Database,
                                                                  delta: Database)
                                                                 (using op: IROp[O, R]): O[T, R[T]] =
  val rules = idb(predicate)
  var result = op.lift[T, R[T]](summon[Relation[R]].empty[T](predicate.arity))
  for rule <- rules do {
    val baseList = rule.body.map { it =>
      for
        base <- op.scan[T](base, PredicateWithArity(it.predicate, it.arity))
        derived <- op.scan[T](derived, PredicateWithArity(it.predicate, it.arity))
      yield base.union(derived)
    }
    val deltaList = rule.body.map { it =>
      // Negation needs base facts to be present in the delta.
      for
        base <- op.scan[T](base, PredicateWithArity(it.predicate, it.arity))
        delta <- op.scan[T](delta, PredicateWithArity(it.predicate, it.arity))
      yield base.union(delta)
    }
    result = for
      r <- result
      baseOps <- baseList.toOp
      deltaOps <- deltaList.toOp
      res = evalRuleIncremental(rule, baseOps, deltaOps)
    yield r.union(res)
  }
  result.map(_.distinct())

// TODO : Document this.
def naiveEval[O[_, _], R[_] : Relation, T: Context](idb: RulesDatabase[T],
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
  op.doWhileNotEqual[T, Unit](
    op.sequence(sequence.toList),
    result,
    copy,
  )

// TODO : Document this.
def semiNaiveEval[O[_, _], R[_] : Relation, T: Context](idb: RulesDatabase[T], base: Database, result: Database)
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
    inner += op.store(delta, p,
      for
        f <- facts
        e <- existing
      yield f.minus(e)
    )
  }
  idb.iterator.foreach { p =>
    val current = op.scan[T](result, p)
    val newFacts = op.scan[T](delta, p)
    inner += op.store(result, p,
      for
        c <- current
        f <- newFacts
      yield c.union(f)
    )
  }

  outer += op.doWhileNonEmpty(
    op.sequence(inner.result()),
    delta
  )

  op.sequence(outer.result())
