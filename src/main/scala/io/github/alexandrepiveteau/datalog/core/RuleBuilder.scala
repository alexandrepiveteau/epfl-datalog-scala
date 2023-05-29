package io.github.alexandrepiveteau.datalog.core

import io.github.alexandrepiveteau.datalog.core.rule.{Atom, Predicate, Variable}

// TODO : Document this.
trait RuleBuilder[-T]:

  // TODO : Document this.
  def predicate(predicate: Predicate, atoms: List[Atom[T]], negated: Boolean): Unit

  // TODO : Document this.
  def aggregate(agg: AggregationFunction, same: Iterable[Variable], column: Iterable[Variable], value: Variable): Unit
