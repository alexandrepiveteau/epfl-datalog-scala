package io.github.alexandrepiveteau.datalog.core.interpreter.ir

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.{AggregationColumn, Column, Index, TupleSet, given}
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, StorageManager, nonEmpty}
import io.github.alexandrepiveteau.datalog.core.rule.{Predicate, Value}
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

import scala.quoted.*

/**
 * An [[IROp]] is an operation that can be performed in the abstract interpreter. All programs are solved as a
 * sequence of calls to some [[IROp]]s, which are then interpreted.
 *
 * The result of the application of [[IROp]]s is given when the [[compute()]] operation is called.
 *
 * @tparam O the type of the operations, which work on constants and return values.
 * @tparam R the type of the relations.
 */
trait IROp[O[_, _], R[_]]:

  /**
   * Sequences multiple [[IROp2]] together.
   *
   * @param ops the operations to sequence.
   * @tparam T the type of the operations.
   * @return a new [[IROp2]] which sequences the given operations.
   */
  def sequence[C](ops: List[O[C, Unit]]): O[C, Unit]

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
  def doWhileNotEqual[C](op: O[C, Unit], first: Database, second: Database): O[C, Unit]

  /**
   * Performs the operation until the database is empty.
   *
   * @param op       the operation to perform.
   * @param database the database to load the relation from.
   * @tparam T the type of the constants in the relation.
   * @return a new [[IROp2]] which loads the relation from the database.
   */
  def doWhileNonEmpty[C](op: O[C, Unit], database: Database): O[C, Unit]

  // TODO : Provide finer-grained operations for the following.

  /**
   * Merges the [[Database.Result]] into [[Database.Base]], and clears the [[Database.Result]].
   *
   * @return a new [[IROp2]] which merges the result into the base.
   */
  def mergeAndClear[C](): O[C, Unit]

  // TODO : It is actually a mistake to have Relation. We must have all ops in this interface so the operations such as
  //        union do not need a Monad to be implemented in an IROp context.
  //        This is also how we get rid of lifting etc. and we can implement a compiled interpreter.

  def empty[C](arity: Int): O[C, R[C]]

  def domain[C](arity: Int, values: Set[Value[C]]): O[C, R[C]]

  def join[C](relations: List[O[C, R[C]]]): O[C, R[C]]

  extension[C] (relation: O[C, R[C]])

    def arity: O[C, Int]

    def aggregate(projection: List[AggregationColumn[C]],
                  same: Set[Index],
                  aggregate: AggregationFunction,
                  indices: Set[Index],
                 )(using domain: Domain[C]): O[C, R[C]]

    def minus(other: O[C, R[C]]): O[C, R[C]]

    def union(other: O[C, R[C]]): O[C, R[C]]

    def distinct(): O[C, R[C]]

    def project(projection: List[Column[C]]): O[C, R[C]]

    def select(selection: Set[Set[Column[C]]]): O[C, R[C]]

