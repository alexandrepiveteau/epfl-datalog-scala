package io.github.alexandrepiveteau.datalog.core.interpreter.algebra

import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Value}
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

import scala.collection.mutable

// TODO : Document this.
case class TupleSet[T](arity: Int, tuples: Set[Fact[T]]):

  def foreach(f: Fact[T] => Unit): Unit = tuples.foreach(f)

  // TODO : Document this.
  def join(other: TupleSet[T]): TupleSet[T] =
    buildRelation(arity + other.arity)(foreach(a => other.foreach(b => add(a ++ b))))

object TupleSet:

  def empty[T](arity: Int): TupleSet[T] =
    TupleSet(arity, Set.empty)

  def domain[T](arity: Int, domain: Set[Value[T]]): TupleSet[T] =
    val column = TupleSet(1, domain.map(List(_)))
    var result = if arity == 0 then TupleSet(0, Set.empty[Fact[T]]) else column
    for _ <- 1 until arity do result = result.join(column)
    result

  def join[T](relations: List[TupleSet[T]]): TupleSet[T] =
    val iterator = relations.iterator
    var result = iterator.next()
    while (iterator.hasNext) result = result.join(iterator.next())
    result

  def union[T](relations: Set[TupleSet[T]]): TupleSet[T] =
    val iterator = relations.iterator
    val first = iterator.next()
    buildRelation(first.arity) {
      first.foreach(add)
      while (iterator.hasNext) iterator.next().foreach(add)
    }

  def arity[T](relation: TupleSet[T]): Int =
    relation.arity

  def aggregate[T](relation: TupleSet[T],
                   projection: List[AggregationColumn[T]],
                   same: Set[Index],
                   aggregate: AggregationFunction,
                   indices: Set[Index])
                  (using domain: Domain[T]): TupleSet[T] =
    buildRelation(projection.size) {
      val result = mutable.Map.empty[Set[Value[T]], Fact[T]]
      TupleSet.distinct(relation).foreach { term =>
        val key = same.map { it => term(it.index) }
        val existing = result.get(key)
        val value = projection.map { it => term.value(it, indices, aggregate) }
        val updated =
          existing
            .map(it => merge(it, value, aggregate, projection))
            .getOrElse(value)
        result.update(key, updated)
      }
      result.values.foreach(add)
    }

  def minus[T](relation: TupleSet[T], other: TupleSet[T]): TupleSet[T] =
    val tuples = relation
    val otherTuples = other
    buildRelation(tuples.arity) {
      tuples.foreach { row =>
        if (!otherTuples.tuples.contains(row)) {
          add(row)
        }
      }
    }

  def distinct[T](relation: TupleSet[T]): TupleSet[T] =
    val tuples = relation
    buildRelation(tuples.arity) {
      val seen = mutable.Set.empty[Fact[T]]
      tuples.foreach { row =>
        if (!seen.contains(row)) {
          seen.add(row)
          add(row)
        }
      }
    }

  def project[T](relation: TupleSet[T], columns: List[Column[T]]): TupleSet[T] =
    buildRelation(columns.size) {
      relation.foreach { row =>
        add(columns.map(row.value))
      }
    }

  def select[T](relation: TupleSet[T], selection: Set[Set[Column[T]]]): TupleSet[T] =
    val list = selection.toList.map(_.toList)
    val tuples = relation
    buildRelation(tuples.arity) {
      tuples.foreach { row =>
        val insert = list.forall {
          g => g.forall(c => row.value(c) == row.value(g.head))
        }
        if (insert) add(row)
      }
    }


// TODO : Document this.
private def buildRelation[T](arity: Int)(builder: mutable.Set[Fact[T]] ?=> Unit): TupleSet[T] =
  val facts = mutable.Set.empty[Fact[T]]
  builder(using facts)
  TupleSet(arity, facts.toSet)

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
