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
given InterpreterIROp[C]: IROp[C, [R] =>> IROpInterpreter[C, R], TupleSet] with

  override def sequence(ops: List[IROpInterpreter[C, Unit]]): IROpInterpreter[C, Unit] = s =>
    ops.foreach(_.apply(s))

  override def scan(database: Database, predicate: PredicateWithArity): IROpInterpreter[C, TupleSet[C]] = s =>
    s.database(database).apply(predicate)

  override def store(database: Database,
                        predicate: PredicateWithArity,
                        relation: IROpInterpreter[C, TupleSet[C]]): IROpInterpreter[C, Unit] = s =>
    s.database(database).update(predicate, relation.apply(s))

  override def doWhileNotEqual(op: IROpInterpreter[C, Unit],
                                  first: Database,
                                  second: Database): IROpInterpreter[C, Unit] = s =>
    while
      op.apply(s)
      s.database(first) != s.database(second)
    do ()

  override def doWhileNonEmpty(op: IROpInterpreter[C, Unit],
                                  database: Database): IROpInterpreter[C, Unit] = s =>
    while
      op.apply(s)
      s.database(database).nonEmpty
    do ()

  override def mergeAndClear(): IROpInterpreter[C, Unit] = s =>
    s.database(Database.Base) += s.database(Database.Result)
    s.removeAll(Set(Database.Base))

  override def empty(arity: Int): IROpInterpreter[C, TupleSet[C]] = s =>
    TupleSet.empty(arity)

  override def domain(arity: Int, domain: Set[Value[C]]): IROpInterpreter[C, TupleSet[C]] = s =>
    TupleSet.domain(arity, domain)

  override def join(relations: List[IROpInterpreter[C, TupleSet[C]]]): IROpInterpreter[C, TupleSet[C]] = s =>
    TupleSet.join(relations.map(_.apply(s)))

  override def union(relations: Set[IROpInterpreter[C, TupleSet[C]]]): IROpInterpreter[C, TupleSet[C]] = s =>
    TupleSet.union(relations.map(_.apply(s)))

  extension (op: IROpInterpreter[C, TupleSet[C]])

    override def arity: IROpInterpreter[C, Int] = s =>
      TupleSet.arity(op(s))

    override def aggregate(projection: List[AggregationColumn[C]],
                           same: Set[Index],
                           aggregate: AggregationFunction,
                           indices: Set[Index])
                          (using domain: Domain[C]): IROpInterpreter[C, TupleSet[C]] = s =>
      TupleSet.aggregate(op(s), projection, same, aggregate, indices)

    override def minus(other: IROpInterpreter[C, TupleSet[C]]): IROpInterpreter[C, TupleSet[C]] = s =>
      TupleSet.minus(op(s), other(s))

    override def distinct(): IROpInterpreter[C, TupleSet[C]] = s =>
      TupleSet.distinct(op(s))

    override def project(columns: List[Column[C]]): IROpInterpreter[C, TupleSet[C]] = s =>
      TupleSet.project(op(s), columns)

    override def select(selection: Set[Set[Column[C]]]): IROpInterpreter[C, TupleSet[C]] = s =>
      TupleSet.select(op(s), selection)

