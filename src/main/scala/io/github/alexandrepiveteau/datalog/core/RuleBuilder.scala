package io.github.alexandrepiveteau.datalog.core

import io.github.alexandrepiveteau.datalog.core.rule.{Term, Predicate, Variable}

// TODO : Document this.
trait RuleBuilder[-T]:

  // TODO : Document this.
  def predicate(predicate: Predicate, terms: List[Term[T]], negated: Boolean): Unit

  // TODO : Document this.
  def aggregate(agg: AggregationFunction, same: Iterable[Variable], column: Iterable[Variable], value: Variable): Unit

def predicate[T](predicate: Predicate, terms: List[Term[T]], negated: Boolean)(using builder: RuleBuilder[T]): Unit =
  builder.predicate(predicate, terms, negated)

def aggregate[T]
(agg: AggregationFunction, same: Iterable[Variable], column: Iterable[Variable], value: Variable)
(using builder: RuleBuilder[T]): Unit =
  builder.aggregate(agg, same, column, value)
