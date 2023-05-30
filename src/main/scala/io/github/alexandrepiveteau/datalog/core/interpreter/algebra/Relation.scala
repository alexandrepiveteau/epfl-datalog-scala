package io.github.alexandrepiveteau.datalog.core.interpreter.algebra

import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Value}
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

import scala.collection.mutable

// TODO : Document this.
case class Relation[T](arity: Int, tuples: Set[Fact[T]])

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
  def foreach(f: Fact[T] => Unit): Unit = r.tuples.foreach(f)

  // TODO : Document this.
  def join(other: Relation[T]): Relation[T] =
    buildRelation(r.arity + other.arity)(r.foreach(a => other.foreach(b => add(a ++ b))))

  // TODO : Document this.
  def select(selection: Set[Set[Column[T]]]): Relation[T] =
    val list = selection.toList.map(_.toList)
    buildRelation(r.arity) {
      foreach { row =>
        val insert = list.forall {
          g => g.forall(c => row.value(c) == row.value(g.head))
        }
        if (insert) add(row)
      }
    }

  // TODO : Document this.
  def join(others: Iterable[Relation[T]]): Relation[T] =
    others.foldLeft(r)((acc, other) => acc.join(other))

  // TODO : Document this.
  def union(other: Relation[T]): Relation[T] =
    buildRelation(r.arity) {
      r.foreach(add)
      other.foreach(add)
    }

  // TODO : Document this.
  def distinct(): Relation[T] =
    buildRelation(r.arity) {
      val seen = mutable.Set.empty[Fact[T]]
      r.foreach { row =>
        if (!seen.contains(row)) {
          seen.add(row)
          add(row)
        }
      }
    }

  // TODO : Document this.
  def minus(other: Relation[T]): Relation[T] =
    buildRelation(r.arity) {
      r.foreach { row =>
        if (!other.tuples.contains(row)) {
          add(row)
        }
      }
    }

  // TODO : Document this.
  def project(projection: List[Column[T]]): Relation[T] =
    buildRelation(projection.size) {
      r.foreach { row =>
        add(projection.map(row.value))
      }
    }

  // TODO : Document this.
  // TODO : Include the require checks.
  def aggregate(
                 projection: List[AggregationColumn[T]],
                 same: Set[Index],
                 aggregate: AggregationFunction,
                 indices: Set[Index],
               )
               (using domain: Domain[T]): Relation[T] =
    buildRelation(projection.size) {
      val result = mutable.Map.empty[Set[Value[T]], Fact[T]]
      r.distinct().foreach { atom =>
        val key = same.map { it => atom(it.index) }
        val existing = result.get(key)
        val value = projection.map { it => atom.value(it, indices, aggregate) }
        val updated =
          existing
            .map(it => merge(it, value, aggregate, projection))
            .getOrElse(value)
        result.update(key, updated)
      }
      result.values.foreach(add)
    }

// TODO : Document this.
extension[T] (iterable: Iterable[Relation[T]])

  // TODO : Document this.
  def join(): Relation[T] =
    val iterator = iterable.iterator
    var result = iterator.next()
    while (iterator.hasNext) result = result.join(iterator.next())
    result

// TODO : Document this.
private def buildRelation[T](arity: Int)(builder: mutable.Set[Fact[T]] ?=> Unit): Relation[T] =
  val facts = mutable.Set.empty[Fact[T]]
  builder(using facts)
  Relation(arity, facts.toSet)

// TODO : Document this.
private def add[T](fact: Fact[T])(using facts: mutable.Set[Fact[T]]): Unit = facts.add(fact)

// TODO : Document this.

extension[T] (fact: Fact[T])

  // TODO : Document this.
  def value(column: Column[T]): Value[T] =
    column match
      case Constant(value) => value
      case Index(index) => fact(index)

  // TODO : Document this.
  def value(column: AggregationColumn[T], indices: Set[Index], aggregate: AggregationFunction)
           (using domain: Domain[T]): Value[T] =
    column match
      case AggregationColumnColumn(column) => value(column)
      case AggregationColumnAggregate =>
        val set = fact.zipWithIndex
          .map((_, index) => Index(index))
          .filter(index => indices.contains(index))
          .toSet
        aggregate.transform(set, fact)

// TODO : Document this.
private def merge[T](first: Fact[T], second: Fact[T], agg: AggregationFunction, columns: List[AggregationColumn[T]])
                    (using domain: Domain[T]): Fact[T] =
  columns.zipWithIndex
    .map((column, index) =>
      val x = first(index)
      val y = second(index)
      column match
        case AggregationColumnColumn(_) => x
        case AggregationColumnAggregate => agg.combine(x, y)
    )