package io.github.alexandrepiveteau.datalog.core

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.Index
import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Value}

// TODO : Document this.
sealed trait AggregationFunction:

  def transform[T](indices: List[Index], atoms: Fact[T])(using domain: Domain[T]): Value[T]

  def combine[T](a: Value[T], b: Value[T])(using domain: Domain[T]): Value[T]

// TODO : Document this.
case object Count extends AggregationFunction:

  override def transform[T](indices: List[Index], atoms: Fact[T])(using domain: Domain[T]): Value[T] =
    domain.unit

  override def combine[T](a: Value[T], b: Value[T])(using domain: Domain[T]): Value[T] =
    domain.sum(a, b)

// TODO : Document this.
case object Sum extends AggregationFunction:

  override def transform[T](indices: List[Index], atoms: Fact[T])(using domain: Domain[T]): Value[T] =
    atoms.zipWithIndex
      .filter((_, i) => indices.contains(Index(i)))
      .map(_._1)
      .reduce(domain.sum)

  override def combine[T](a: Value[T], b: Value[T])(using domain: Domain[T]): Value[T] =
    domain.sum(a, b)

// TODO : Document this.
case object Min extends AggregationFunction:

  override def transform[T](indices: List[Index], atoms: Fact[T])(using domain: Domain[T]): Value[T] =
    atoms.zipWithIndex
      .filter((_, i) => indices.contains(Index(i)))
      .map(_._1)
      .reduce(domain.min)

  override def combine[T](a: Value[T], b: Value[T])(using domain: Domain[T]): Value[T] =
    domain.min(a, b)

// TODO : Document this.
case object Max extends AggregationFunction:

  override def transform[T](indices: List[Index], atoms: Fact[T])(using domain: Domain[T]): Value[T] =
    atoms.zipWithIndex
      .filter((_, i) => indices.contains(Index(i)))
      .map(_._1)
      .reduce(domain.max)

  override def combine[T](a: Value[T], b: Value[T])(using domain: Domain[T]): Value[T] =
    domain.max(a, b)
