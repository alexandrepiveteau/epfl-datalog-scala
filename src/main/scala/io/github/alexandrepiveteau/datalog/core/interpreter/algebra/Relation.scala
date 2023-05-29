package io.github.alexandrepiveteau.datalog.core.interpreter.algebra

import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Value}

import scala.collection.mutable

// TODO : Document this.
case class Relation[T](arity: Int, facts: Set[Fact[T]])

// TODO : Document this.
object Relation:

  // TODO : Document this.
  def empty[T](arity: Int): Relation[T] = Relation(arity, Set.empty)

  // TODO : Document this.
  def domain[T](arity: Int, domain: Set[Value[T]]): Relation[T] =
    val column = Relation(1, domain.map(List(_)))
    var result = if arity == 0 then empty(0) else column
    for _ <- 1 until arity do result = result.join(column)
    result

// TODO : Document this.
extension[T] (r: Relation[T])

  // TODO : Document this.
  def foreach(f: Fact[T] => Unit): Unit = r.facts.foreach(f)

  // TODO : Document this.
  def join(other: Relation[T]): Relation[T] =
    buildRelation(r.arity + other.arity, facts =>
      r.foreach(a => other.foreach(b => facts.add(a ++ b)))
    )

// TODO : Document this.
def buildRelation[T](arity: Int, builder: mutable.Set[Fact[T]] => Unit): Relation[T] =
  val facts = scala.collection.mutable.Set.empty[Fact[T]]
  builder(facts)
  Relation(arity, facts.toSet)