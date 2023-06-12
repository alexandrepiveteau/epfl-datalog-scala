package io.github.alexandrepiveteau.datalog.core.interpreter.algebra

import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Relation
import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Value}
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

import scala.collection.mutable

// TODO : Document this.
case class TupleSet[T](arity: Int, tuples: Set[Fact[T]]):

  def foreach(f: Fact[T] => Unit): Unit = tuples.foreach(f)

  // TODO : Document this.
  def join(other: TupleSet[T]): TupleSet[T] =
    buildRelation(arity + other.arity)(foreach(a => other.foreach(b => add(a ++ b))))

given Relation[TupleSet] with

  override def empty[T](arity: Int): TupleSet[T] =
    TupleSet(arity, Set.empty)

  override def domain[T](arity: Int, domain: Set[Value[T]]): TupleSet[T] =
    val column = TupleSet(1, domain.map(List(_)))
    var result = if arity == 0 then empty(0) else column
    for _ <- 1 until arity do result = result.join(column)
    result

  override def join[T](relations: List[TupleSet[T]]): TupleSet[T] =
    val iterator = relations.iterator
    var result = iterator.next()
    while (iterator.hasNext) result = result.join(iterator.next())
    result

  extension[T] (r: TupleSet[T])

    override def arity: Int = r.arity

    override def aggregate(projection: List[AggregationColumn[T]],
                           same: Set[Index],
                           aggregate: AggregationFunction,
                           indices: Set[Index])
                          (using domain: Domain[T]): TupleSet[T] =
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

    override def minus(other: TupleSet[T]): TupleSet[T] =
      buildRelation(r.arity) {
        r.foreach { row =>
          if (!other.tuples.contains(row)) {
            add(row)
          }
        }
      }

    override def union(other: TupleSet[T]): TupleSet[T] =
      buildRelation(r.arity) {
        r.foreach(add)
        other.foreach(add)
      }

    override def distinct(): TupleSet[T] =
      buildRelation(r.arity) {
        val seen = mutable.Set.empty[Fact[T]]
        r.foreach { row =>
          if (!seen.contains(row)) {
            seen.add(row)
            add(row)
          }
        }
      }

    override def project(columns: List[Column[T]]): TupleSet[T] =
      buildRelation(columns.size) {
        r.foreach { row =>
          add(columns.map(row.value))
        }
      }

    override def select(selection: Set[Set[Column[T]]]): TupleSet[T] =
      val list = selection.toList.map(_.toList)
      buildRelation(r.arity) {
        r.foreach { row =>
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
