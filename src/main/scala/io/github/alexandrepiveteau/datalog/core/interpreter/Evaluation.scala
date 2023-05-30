package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.*
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.*
import io.github.alexandrepiveteau.datalog.core.rule.*

import scala.collection.mutable

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
private def evalRule[T](rule: Rule[T], relations: List[IRRelOp[T]])
                       (using context: Context[T]): IRRelOp[T] =
  rule match
    case rule: CombinationRule[T] => evalCombinationRule(rule, relations)
    case rule: AggregationRule[T] => evalAggregationRule(rule, relations.head)

// TODO : Document this.
private def evalCombinationRule[T](rule: CombinationRule[T], relations: List[IRRelOp[T]])
                                  (using context: Context[T]): IRRelOp[T] =
  if rule.body.size != relations.size then throw IllegalArgumentException("Invalid number of relations.")
  // 1. Negate all the relations that are negated in the rule.
  // 2. Generate a concatenation of all atoms in the rule, after the join.
  // 3. Join all the relations.
  // 4. Select the rows that match the constants and variables.
  // 5. Project the rows to the correct indices, and add constants to the projection.
  val negated = relations.zipWithIndex
    .map((r, idx) => if rule.body(idx).negated then r.negated else r)
  val concat = rule.body.flatMap(_.atoms.toList)
  IRRelProject(IRRelSelect(IRRelJoin(negated), selection(concat)), projection(rule.head.atoms, concat))

// TODO : Document this.
private def evalAggregationRule[T](rule: AggregationRule[T], relation: IRRelOp[T])
                                  (using context: Context[T]): IRRelOp[T] =
  // 1. Negate the relation if the rule is negated.
  // 2. Perform the aggregation.
  val negated = if rule.clause.negated then relation.negated else relation
  val projection = rule.head.atoms.map {
    case atom: Value[T] => AggregationColumnColumn(Constant(atom))
    case atom: Variable =>
      if atom == rule.aggregate.result then AggregationColumnAggregate
      else AggregationColumnColumn(Index(rule.clause.atoms.indexOf(atom)))
  }
  val same = rule.aggregate.same.toSet.map(it => Index(rule.clause.atoms.indexOf(it)))
  val indices = rule.aggregate.columns.toSet.map(it => Index(rule.clause.atoms.indexOf(it)))
  IRRelAggregate(
    relation = negated,
    projection = projection,
    same = same,
    domain = context.domain,
    aggregate = rule.aggregate.agg,
    indices = indices,
  )

// TODO : Document this.
private def evalRuleIncremental[T](rule: Rule[T], relations: List[IRRelOp[T]], incremental: List[IRRelOp[T]])
                                  (using context: Context[T]): IRRelOp[T] =
  if rule.body.size != relations.size then throw IllegalArgumentException("Invalid number of relations.")
  if rule.body.size != incremental.size then throw IllegalArgumentException("Invalid number of relations.")
  var result: IRRelOp[T] = IRRelEmpty[T](rule.head.arity)
  for i <- rule.body.indices do {
    val args = List.range(0, rule.body.size).map { it => if (it == i) incremental(it) else relations(it) }
    result = IRRelUnion(result, evalRule(rule, args))
  }
  IRRelDistinct(result)

// TODO : Document this.
private def eval[T](predicate: PredicateWithArity, idb: RulesDatabase[T], base: Database, derived: Database)
                   (using context: Context[T]): IRRelOp[T] =
  val rules = idb(predicate)
  var result: IRRelOp[T] = IRRelEmpty(predicate.arity)
  for rule <- rules do {
    val list = rule.body.map { it =>
      val baseScan = IRRelScan[T](base, PredicateWithArity(it.predicate, it.arity))
      val derivedScan = IRRelScan[T](derived, PredicateWithArity(it.predicate, it.arity))
      IRRelUnion(baseScan, derivedScan)
    }
    result = IRRelUnion(result, evalRule(rule, list))
  }
  IRRelDistinct(result)

// TODO : Document this.
private def evalIncremental[T](predicate: PredicateWithArity, idb: RulesDatabase[T], base: Database, derived: Database, delta: Database)
                              (using context: Context[T]): IRRelOp[T] =
  val rules = idb(predicate)
  var result: IRRelOp[T] = IRRelEmpty(predicate.arity)
  for rule <- rules do {
    val baseList = rule.body.map { it =>
      val baseScan = IRRelScan[T](base, PredicateWithArity(it.predicate, it.arity))
      val derivedScan = IRRelScan[T](derived, PredicateWithArity(it.predicate, it.arity))
      IRRelUnion(baseScan, derivedScan)
    }
    val deltaList = rule.body.map { it =>
      // Negation needs base facts to be present in the delta.
      val baseScan = IRRelScan[T](base, PredicateWithArity(it.predicate, it.arity))
      val deltaScan = IRRelScan[T](delta, PredicateWithArity(it.predicate, it.arity))
      IRRelUnion(baseScan, deltaScan)
    }
    result = IRRelUnion(result, evalRuleIncremental(rule, baseList, deltaList))
  }
  IRRelDistinct(result)

// TODO : Document this.
private def naiveEval[T](idb: RulesDatabase[T], base: Database, result: Database)
                        (using context: Context[T]): IROp[T] =
  val copy = Database("Copy")
  val sequence = mutable.ListBuffer[IROp[T]]()
  idb.iterator.foreach { predicate =>
    sequence += IRStore(copy, predicate, IRRelScan(result, predicate))
  }
  idb.iterator.foreach { predicate =>
    val res = eval(predicate, idb, base, result)
    sequence += IRStore(result, predicate, res)
  }
  IRDoWhileNotEqual(
    IRSequence(sequence.toList),
    result,
    copy,
  )

// TODO : Document this.
private def semiNaiveEval[T](idb: RulesDatabase[T], base: Database, result: Database)
                            (using context: Context[T]): IROp[T] =
  val delta = Database("Delta")
  val copy = Database("Copy")
  val outer = mutable.ListBuffer[IROp[T]]()
  val inner = mutable.ListBuffer[IROp[T]]()

  idb.iterator.foreach { predicate =>
    val res = eval(predicate, idb, base, Database.Empty)
    outer += IRStore(result, predicate, res)
    outer += IRStore(delta, predicate, res)
  }

  idb.iterator.foreach { p => inner += IRStore(copy, p, IRRelScan(delta, p)) }
  idb.iterator.foreach { p =>
    val facts = evalIncremental(p, idb, base, result, copy)
    val existing = IRRelScan[T](result, p)
    inner += IRStore(delta, p, IRRelMinus(facts, existing))
  }
  idb.iterator.foreach { p =>
    val current = IRRelScan[T](result, p)
    val newFacts = IRRelScan[T](delta, p)
    inner += IRStore(result, p, IRRelUnion(current, newFacts))
  }

  outer += IRDoWhileNotEmpty(
    IRSequence(inner.toList),
    delta
  )

  IRSequence(outer.toList)
