package io.github.alexandrepiveteau.datalog.core.rule

import io.github.alexandrepiveteau.datalog.core.AggregationFunction

// TODO : Document this.
// TODO : Rename this to Term.
sealed trait Term[+T]

// TODO : Document this.
case class Value[+T](value: T) extends Term[T]

// TODO : Document this.
case class Variable(value: String) extends Term[Nothing]

// TODO : Document this.
case class Predicate(value: String)

// TODO : Document this.
sealed trait Literal[+T]:
  val predicate: Predicate
  val terms: List[Term[T]]
  val arity: Int = terms.size

// TODO : Document this.
case class HeadLiteral[+T](predicate: Predicate, terms: List[Term[T]]) extends Literal[T]

// TODO : Document this.
case class BodyLiteral[+T](predicate: Predicate, terms: List[Term[T]], negated: Boolean) extends Literal[T]

// TODO : Document this.
sealed trait Rule[+T]:
  val head: HeadLiteral[T]
  val body: List[BodyLiteral[T]]

// TODO : Document this.
case class CombinationRule[+T](head: HeadLiteral[T], body: List[BodyLiteral[T]]) extends Rule[T]

// TODO : Document this.
case class Aggregate(agg: AggregationFunction, same: Iterable[Variable], columns: Iterable[Variable], result: Variable)

// TODO : Document this.
case class AggregationRule[+T](head: HeadLiteral[T], clause: BodyLiteral[T], aggregate: Aggregate) extends Rule[T]:
  override val body: List[BodyLiteral[T]] = List(clause)

// TODO : Document this.
type Fact[+T] = List[Value[T]]
