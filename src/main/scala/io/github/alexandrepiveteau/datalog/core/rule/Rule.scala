package io.github.alexandrepiveteau.datalog.core.rule

import io.github.alexandrepiveteau.datalog.core.AggregationFunction

// TODO : Document this.
// TODO : Rename this to Term.
sealed trait Atom[+T]

// TODO : Document this.
case class Value[+T](value: T) extends Atom[T]

// TODO : Document this.
case class Variable(value: Any) extends Atom[Nothing]

// TODO : Document this.
case class Predicate(value: Any)

// TODO : Document this.
// TODO : Rename this to Atom?
sealed trait Literal[+T]:
  val predicate: Predicate
  val atoms: List[Atom[T]]
  val arity: Int = atoms.size

// TODO : Document this.
case class HeadLiteral[+T](predicate: Predicate, atoms: List[Atom[T]]) extends Literal[T]

// TODO : Document this.
case class BodyLiteral[+T](predicate: Predicate, atoms: List[Atom[T]], negated: Boolean) extends Literal[T]

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
