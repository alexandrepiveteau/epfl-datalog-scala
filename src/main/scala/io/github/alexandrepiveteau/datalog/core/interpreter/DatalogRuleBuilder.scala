package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.rule.*
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, RuleBuilder}

import scala.collection.mutable

// TODO : Document this.
class DatalogRuleBuilder[T] extends RuleBuilder[T]:

  // TODO : Document this.
  private case class StoredPredicate(predicate: Predicate, terms: List[Term[T]], negated: Boolean)

  // TODO : Document this.
  private val predicates = mutable.Set[StoredPredicate]()

  // TODO : Document this.
  private val aggregates = mutable.Set[Aggregate]()

  override def predicate(predicate: Predicate, terms: List[Term[T]], negated: Boolean): Unit =
    predicates.add(StoredPredicate(predicate, terms, negated))

  override def aggregate(agg: AggregationFunction, same: Iterable[Variable], column: Iterable[Variable], value: Variable): Unit =
    aggregates.add(Aggregate(agg, same, column, value))

  // TODO : Document this.
  def toRule(predicate: Predicate, terms: List[Term[T]]): Rule[T] =
    if aggregates.isEmpty then toCombinationRule(predicate, terms)
    else toAggregationRule(predicate, terms)

  // TODO : Document this.
  private def toCombinationRule(predicate: Predicate, terms: List[Term[T]]): CombinationRule[T] =
    if aggregates.nonEmpty then throw new IllegalArgumentException("Aggregates should be empty for combination rules.")
    val clauses = predicates.toList.map(it => BodyLiteral(it.predicate, it.terms, it.negated))
    CombinationRule(HeadLiteral(predicate, terms), clauses)

  // TODO : Document this.
  private def toAggregationRule(predicate: Predicate, terms: List[Term[T]]): AggregationRule[T] =
    if aggregates.size != 1 then throw new IllegalArgumentException("Aggregated rules should must have exactly one aggregate.")
    if predicates.size != 1 then throw new IllegalArgumentException("Aggregated rules should must have exactly one clause.")
    val aggregate = aggregates.head
    val clause = predicates.head
    val literal = BodyLiteral(clause.predicate, clause.terms, clause.negated)
    AggregationRule(HeadLiteral(predicate, terms), literal, aggregate)



