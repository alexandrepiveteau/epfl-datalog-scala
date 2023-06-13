package io.github.alexandrepiveteau.datalog.core.interpreter.engine.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.*
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, StorageManager, nonEmpty}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{Database, IROp}
import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Predicate, Value}
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

import scala.collection.mutable

type IROpInterpreter[C, R] = StorageManager[C] => R

/**
 * An implementation of [[IROp]] which returns a direct interpreter, which does not use any kind of
 * intermediate representation. This is the most direct way to execute a program, but it may also miss some
 * optimizations which are possible with a materialized intermediate representation.
 */
given InterpreterIROp: IROp[IROpInterpreter, TupleSet] with

  override def sequence[C](ops: List[IROpInterpreter[C, Unit]]): IROpInterpreter[C, Unit] = s =>
    ops.foreach(_.apply(s))

  override def scan[C](database: Database, predicate: PredicateWithArity): IROpInterpreter[C, TupleSet[C]] = s =>
    s.database(database).apply(predicate)

  override def store[C](database: Database,
                        predicate: PredicateWithArity,
                        relation: IROpInterpreter[C, TupleSet[C]]): IROpInterpreter[C, Unit] = s =>
    s.database(database).update(predicate, relation.apply(s))

  override def doWhileNotEqual[C](op: IROpInterpreter[C, Unit],
                                  first: Database,
                                  second: Database): IROpInterpreter[C, Unit] = s =>
    while
      op.apply(s)
      s.database(first) != s.database(second)
    do ()

  override def doWhileNonEmpty[C](op: IROpInterpreter[C, Unit],
                                  database: Database): IROpInterpreter[C, Unit] = s =>
    while
      op.apply(s)
      s.database(database).nonEmpty
    do ()

  override def mergeAndClear[C](): IROpInterpreter[C, Unit] = s =>
    s.database(Database.Base) += s.database(Database.Result)
    s.removeAll(Set(Database.Base))

  override def empty[T](arity: Int): IROpInterpreter[T, TupleSet[T]] = s =>
    TupleSet.empty(arity)

  override def domain[T](arity: Int, domain: Set[Value[T]]): IROpInterpreter[T, TupleSet[T]] = s =>
    TupleSet.domain(arity, domain)

  override def join[T](relations: List[IROpInterpreter[T, TupleSet[T]]]): IROpInterpreter[T, TupleSet[T]] = s =>
    TupleSet.join(relations.map(_.apply(s)))

  extension[T] (op: IROpInterpreter[T, TupleSet[T]])

    override def arity: IROpInterpreter[T, Int] = s =>
      TupleSet.arity(op(s))

    override def aggregate(projection: List[AggregationColumn[T]],
                           same: Set[Index],
                           aggregate: AggregationFunction,
                           indices: Set[Index])
                          (using domain: Domain[T]): IROpInterpreter[T, TupleSet[T]] = s =>
      TupleSet.aggregate(op(s), projection, same, aggregate, indices)

    override def minus(other: IROpInterpreter[T, TupleSet[T]]): IROpInterpreter[T, TupleSet[T]] = s =>
      TupleSet.minus(op(s), other(s))

    override def union(other: IROpInterpreter[T, TupleSet[T]]): IROpInterpreter[T, TupleSet[T]] = s =>
      TupleSet.union(op(s), other(s))

    override def distinct(): IROpInterpreter[T, TupleSet[T]] = s =>
      TupleSet.distinct(op(s))

    override def project(columns: List[Column[T]]): IROpInterpreter[T, TupleSet[T]] = s =>
      TupleSet.project(op(s), columns)

    override def select(selection: Set[Set[Column[T]]]): IROpInterpreter[T, TupleSet[T]] = s =>
      TupleSet.select(op(s), selection)

