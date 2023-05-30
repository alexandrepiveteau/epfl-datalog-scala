package io.github.alexandrepiveteau.datalog.core.interpreter.algebra

import io.github.alexandrepiveteau.datalog.core.rule.Value

// TODO : Document this.
sealed trait Column[+T]

// TODO : Document this.
case class Constant[+T](value: Value[T]) extends Column[T]

// TODO : Document this.
case class Index(index: Int) extends Column[Nothing]
