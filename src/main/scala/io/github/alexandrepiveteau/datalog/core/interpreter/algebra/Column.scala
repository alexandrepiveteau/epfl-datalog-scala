package io.github.alexandrepiveteau.datalog.core.interpreter.algebra

// TODO : Document this.
sealed trait Column[+T]

// TODO : Document this.
case class Constant[+T](value: T) extends Column[T]

// TODO : Document this.
case class Index(index: Int) extends Column[Nothing]
