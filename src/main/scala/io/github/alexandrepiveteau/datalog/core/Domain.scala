package io.github.alexandrepiveteau.datalog.core

import io.github.alexandrepiveteau.datalog.core.rule.Value

// TODO : Document this.
trait Domain[N] {

  // TODO : Document this.
  def unit: Value[N]

  // TODO : Document this.
  def sum(lhs: Value[N], rhs: Value[N]): Value[N]

  // TODO : Document this.
  def max(lhs: Value[N], rhs: Value[N]): Value[N]

  // TODO : Document this.
  def min(lhs: Value[N], rhs: Value[N]): Value[N]
}
