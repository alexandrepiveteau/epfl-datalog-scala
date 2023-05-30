package io.github.alexandrepiveteau.datalog.core.interpreter.algebra

// TODO : Document this.
sealed trait AggregationColumn[+T]

// TODO : Document this.
case object AggregationColumnAggregate extends AggregationColumn[Nothing]

// TODO : Document this.
case class AggregationColumnColumn[+T](column: Column[T]) extends AggregationColumn[T]
