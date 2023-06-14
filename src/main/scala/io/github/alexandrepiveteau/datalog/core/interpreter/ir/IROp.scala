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
   * @tparam C the type of the operations.
   * @return a new [[IROp]] which sequences the given operations.
   */
  def sequence[C](ops: List[O[C, Unit]]): O[C, Unit]

  /**
   * Scans the given database for the given predicate, and returns the relation.
   *
   * @param database  the database to scan.
   * @param predicate the predicate to scan.
   * @tparam C the type of the constants in the relation.
   * @return a new [[R]] which has loaded the relation from the database.
   */
  def scan[C](database: Database, predicate: PredicateWithArity): O[C, R[C]]

  /**
   * Stores a relation in the given database, replacing the previous value.
   *
   * @param database  the database to store the relation in.
   * @param predicate the predicate to store the relation at.
   * @param relation  the relation to store.
   * @tparam C the type of the constants in the relation.
   * @return a new [[IROp]] which stores the relation in the database.
   */
  def store[C](database: Database, predicate: PredicateWithArity, relation: O[C, R[C]]): O[C, Unit]

  /**
   * Performs the operation until the two databases are equal.
   *
   * @param op     the operation to perform.
   * @param first  the first database to load the relation from.
   * @param second the second database to load the relation from.
   * @tparam C the type of the constants in the relation.
   * @return a new [[IROp]] which loads the relation from the database.
   */
  def doWhileNotEqual[C](op: O[C, Unit], first: Database, second: Database): O[C, Unit]

  /**
   * Performs the operation until the database is empty.
   *
   * @param op       the operation to perform.
   * @param database the database to load the relation from.
   * @tparam C the type of the constants in the relation.
   * @return a new [[IROp]] which loads the relation from the database.
   */
  def doWhileNonEmpty[C](op: O[C, Unit], database: Database): O[C, Unit]

  // TODO : Provide finer-grained operations for the following.

  /**
   * Merges the [[Database.Result]] into [[Database.Base]], and clears the [[Database.Result]].
   *
   * @return a new [[IROp]] which merges the result into the base.
   */
  def mergeAndClear[C](): O[C, Unit]

  /**
   * Creates a new relation with the given arity, and no tuples.
   *
   * @param arity the arity of the relation.
   * @tparam C the type of the relation.
   * @return a new relation with the given arity, and no tuples.
   */
  def empty[C](arity: Int): O[C, R[C]]

  /**
   * Creates a new relation with the given arity and all tuples that can be formed
   * using [[values]].
   *
   * @param arity  the arity of the relation.
   * @param values the values to use to create the tuples.
   * @tparam C the type of the relation.
   * @return a new relation with the given arity and all tuples that can be formed
   */
  def domain[C](arity: Int, values: Set[Value[C]]): O[C, R[C]]

  /**
   * Creates a new relation with the cross product of the given relations.
   *
   * @param relations the relations to cross product.
   * @tparam C the type of the relation.
   * @return a new relation with the cross product of the given relations.
   */
  def join[C](relations: List[O[C, R[C]]]): O[C, R[C]]

  /**
   * Returns the tuples which are present either in the [[relations]].
   *
   * @param relations the relations to union.
   * @return the union of the [[relations]].
   */
  def union[C](relations: Set[O[C, R[C]]]): O[C, R[C]]

  extension[C] (relation: O[C, R[C]])

    /**
     * Returns the arity of the relation.
     */
    def arity: O[C, Int]

    /**
     * Aggregates the relation to a set of values.
     *
     * @param projection the columns to project.
     * @param same       the columns to group by.
     * @param aggregate  the aggregation function to use.
     * @param indices    the indices to use.
     * @param domain     the domain of the relation.
     * @return the aggregated relation.
     */
    def aggregate(projection: List[AggregationColumn[C]],
                  same: Set[Index],
                  aggregate: AggregationFunction,
                  indices: Set[Index],
                 )(using domain: Domain[C]): O[C, R[C]]

    /**
     * Returns the tuples in the [[relation]] which are not present in [[other]].
     *
     * @param other the relation to subtract from the [[relation]].
     * @return the difference between [[relation]] and [[other]].
     */
    def minus(other: O[C, R[C]]): O[C, R[C]]

    /**
     * Returns a relation with only distinct tuples.
     */
    def distinct(): O[C, R[C]]

    /**
     * Projects the relation using the given columns.
     *
     * @param projection the columns to project.
     * @return a new relation with the given columns.
     */
    def project(projection: List[Column[C]]): O[C, R[C]]

    /**
     * Selects the tuples in the relation which match the given selection.
     *
     * @param selection the selection to apply.
     * @return a new relation with the selected tuples.
     */
    def select(selection: Set[Set[Column[C]]]): O[C, R[C]]

