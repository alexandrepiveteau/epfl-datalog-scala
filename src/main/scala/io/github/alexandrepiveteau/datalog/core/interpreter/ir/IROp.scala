package io.github.alexandrepiveteau.datalog.core.interpreter.ir

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.{AggregationColumn, Column, Index, TupleSet, given}
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, StorageManager, nonEmpty}
import io.github.alexandrepiveteau.datalog.core.rule.{Predicate, Value}
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

/**
 * An [[IROp]] is an operation that can be performed in the abstract interpreter. All programs are solved as a
 * sequence of calls to some [[IROp]]s, which are then interpreted.
 *
 * The result of the application of [[IROp]]s is given when the [[compute()]] operation is called.
 *
 * @tparam O the type of the operations, which work on constants and return values.
 * @tparam R the type of the relations.
 */
trait IROp[O[_, _], R[_] : Relation]:

  /**
   * Sequences multiple [[IROp2]] together.
   *
   * @param ops the operations to sequence.
   * @tparam T the type of the operations.
   * @return a new [[IROp2]] which sequences the given operations.
   */
  def sequence[C, T](ops: List[O[C, T]]): O[C, Unit]

  /**
   * Scans the given database for the given predicate, and returns the relation.
   *
   * @param database  the database to scan.
   * @param predicate the predicate to scan.
   * @tparam T the type of the constants in the relation.
   * @return a new [[R]] which has loaded the relation from the database.
   */
  def scan[C](database: Database, predicate: PredicateWithArity): O[C, R[C]]

  /**
   * Stores a relation in the given database, replacing the previous value.
   *
   * @param database  the database to store the relation in.
   * @param predicate the predicate to store the relation at.
   * @param relation  the relation to store.
   * @tparam T the type of the constants in the relation.
   * @return a new [[IROp2]] which stores the relation in the database.
   */
  def store[C](database: Database, predicate: PredicateWithArity, relation: O[C, R[C]]): O[C, Unit]

  /**
   * Performs the operation until the two databases are equal.
   *
   * @param op     the operation to perform.
   * @param first  the first database to load the relation from.
   * @param second the second database to load the relation from.
   * @tparam T the type of the constants in the relation.
   * @return a new [[IROp2]] which loads the relation from the database.
   */
  def doWhileNotEqual[C, T](op: O[C, T], first: Database, second: Database): O[C, Unit]

  /**
   * Performs the operation until the database is empty.
   *
   * @param op       the operation to perform.
   * @param database the database to load the relation from.
   * @tparam T the type of the constants in the relation.
   * @return a new [[IROp2]] which loads the relation from the database.
   */
  def doWhileNonEmpty[C, T](op: O[C, T], database: Database): O[C, Unit]

  // TODO : Provide finer-grained operations for the following.

  /**
   * Merges the [[Database.Result]] into [[Database.Base]], and clears the [[Database.Result]].
   *
   * @return a new [[IROp2]] which merges the result into the base.
   */
  def mergeAndClear[C](): O[C, Unit]

  // Monadic support of computations in the IR.
  def lift[A, B](a: B): O[A, B]

  extension[S, B] (op: O[S, B])
    def map[C](f: B => C): O[S, C]
    def flatMap[C](f: B => O[S, C]): O[S, C]

  // Monad transformers in the IR.
  extension[S, B] (ops: List[O[S, B]])
    def toOp: O[S, List[B]]


// INTERPRETATION OF THE IR

type IROpInterpreter[C, R] = StorageManager[C] => R

given IROp[IROpInterpreter, TupleSet] with

  override def sequence[C, T](ops: List[IROpInterpreter[C, T]]): IROpInterpreter[C, Unit] = s =>
    ops.foreach(_.apply(s))

  override def scan[C](database: Database, predicate: PredicateWithArity): IROpInterpreter[C, TupleSet[C]] = s =>
    s.database(database).apply(predicate)

  override def store[C](database: Database,
                        predicate: PredicateWithArity,
                        relation: IROpInterpreter[C, TupleSet[C]]): IROpInterpreter[C, Unit] = s =>
    s.database(database).update(predicate, relation.apply(s))

  override def doWhileNotEqual[C, T](op: IROpInterpreter[C, T],
                                     first: Database,
                                     second: Database): IROpInterpreter[C, Unit] = s =>
    while
      op.apply(s)
      s.database(first) != s.database(second)
    do ()

  override def doWhileNonEmpty[C, T](op: IROpInterpreter[C, T],
                                     database: Database): IROpInterpreter[C, Unit] = s =>
    while
      op.apply(s)
      s.database(database).nonEmpty
    do ()

  override def mergeAndClear[C](): IROpInterpreter[C, Unit] = s =>
    s.database(Database.Base) += s.database(Database.Result)
    s.removeAll(Set(Database.Base))

  override def lift[A, B](a: B): IROpInterpreter[A, B] = _ => a

  extension[S, B] (op: IROpInterpreter[S, B])
    def map[C](f: B => C): IROpInterpreter[S, C] = s => f(op(s))
    def flatMap[C](f: B => IROpInterpreter[S, C]): IROpInterpreter[S, C] = s => f(op(s))(s)

  extension[S, B] (ops: List[IROpInterpreter[S, B]])
    def toOp: IROpInterpreter[S, List[B]] = s => ops.map(_.apply(s))
