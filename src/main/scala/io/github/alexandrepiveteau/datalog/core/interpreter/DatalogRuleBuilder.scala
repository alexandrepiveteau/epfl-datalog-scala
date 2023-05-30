package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.rule.*
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, RuleBuilder}

import scala.collection.mutable

// TODO : Document this.
class DatalogRuleBuilder[T] extends RuleBuilder[T]:

  // TODO : Document this.
  private case class StoredPredicate(predicate: Predicate, atoms: List[Atom[T]], negated: Boolean)

  // TODO : Document this.
  private val predicates = mutable.Set[StoredPredicate]()

  // TODO : Document this.
  private val aggregates = mutable.Set[Aggregate]()

  override def predicate(predicate: Predicate, atoms: List[Atom[T]], negated: Boolean): Unit =
    predicates.add(StoredPredicate(predicate, atoms, negated))

  override def aggregate(agg: AggregationFunction, same: Iterable[Variable], column: Iterable[Variable], value: Variable): Unit =
    aggregates.add(Aggregate(agg, same, column, value))

  // TODO : Document this.
  def toRule(predicate: Predicate, atoms: List[Atom[T]]): Rule[T] =
    if aggregates.isEmpty then toCombinationRule(predicate, atoms)
    else toAggregationRule(predicate, atoms)

  // TODO : Document this.
  private def toCombinationRule(predicate: Predicate, atoms: List[Atom[T]]): CombinationRule[T] =
    if aggregates.nonEmpty then throw new IllegalArgumentException("Aggregates should be empty for combination rules.")
    val clauses = predicates.toList.map(it => BodyLiteral(it.predicate, it.atoms, it.negated))
    CombinationRule(HeadLiteral(predicate, atoms), clauses)

  // TODO : Document this.
  private def toAggregationRule(predicate: Predicate, atoms: List[Atom[T]]): AggregationRule[T] =
    if aggregates.size != 1 then throw new IllegalArgumentException("Aggregated rules should must have exactly one aggregate.")
    if predicates.size != 1 then throw new IllegalArgumentException("Aggregated rules should must have exactly one clause.")
    val aggregate = aggregates.head
    val clause = predicates.head
    val literal = BodyLiteral(clause.predicate, clause.atoms, clause.negated)
    AggregationRule(HeadLiteral(predicate, atoms), literal, aggregate)



