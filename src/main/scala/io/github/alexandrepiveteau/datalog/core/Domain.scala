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

extension (t: Int.type)
  def domain: Domain[Int] = IntDomain

private case object IntDomain extends Domain[Int]:

  override def unit: Value[Int] = Value(1)

  override def sum(lhs: Value[Int], rhs: Value[Int]): Value[Int] = Value(lhs.value + rhs.value)

  override def max(lhs: Value[Int], rhs: Value[Int]): Value[Int] = Value(lhs.value max rhs.value)

  override def min(lhs: Value[Int], rhs: Value[Int]): Value[Int] = Value(lhs.value min rhs.value)
