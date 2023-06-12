package io.github.alexandrepiveteau.datalog.core.interpreter.ir

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.{AggregationColumn, Column, Index}
import io.github.alexandrepiveteau.datalog.core.interpreter.database.StorageManager
import io.github.alexandrepiveteau.datalog.core.rule.{Predicate, Value}
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

/**
 * A relation is a set of tuples, with a certain arity. It is used to represent some computations over the tuples, and
 * describe how the tuples can be combined together.
 *
 * Implementations of this trait may compute the result of the operations eagerly, or lazily. The only requirement is
 * that the result of the operations is deterministic.
 *
 * @tparam R the type of the relation.
 */
trait Relation[R[_]]:

  /**
   * Creates a new relation with the given arity, and no tuples.
   *
   * @param arity the arity of the relation.
   * @tparam T the type of the relation.
   * @return a new relation with the given arity, and no tuples.
   */
  def empty[T](arity: Int): R[T]

  /**
   * Creates a new relation with the given arity and all tuples that can be formed
   * using [[values]].
   *
   * @param arity  the arity of the relation.
   * @param values the values to use to create the tuples.
   * @tparam T the type of the relation.
   * @return a new relation with the given arity and all tuples that can be formed
   */
  def domain[T](arity: Int, values: Set[Value[T]]): R[T]

  /**
   * Creates a new relation with the cross product of the given relations.
   *
   * @param relations the relations to cross product.
   * @tparam T the type of the relation.
   * @return a new relation with the cross product of the given relations.
   */
  def join[T](relations: List[R[T]]): R[T]

  extension[T] (relation: R[T])

    /**
     * Returns the arity of the relation.
     */
    def arity: Int

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
    def aggregate(projection: List[AggregationColumn[T]],
                  same: Set[Index],
                  aggregate: AggregationFunction,
                  indices: Set[Index],
                 )(using domain: Domain[T]): R[T]

    /**
     * Returns the tuples in the [[relation]] which are not present in [[other]].
     *
     * @param other the relation to subtract from the [[relation]].
     * @return the difference between [[relation]] and [[other]].
     */
    def minus(other: R[T]): R[T]

    /**
     * Returns the tuples which are present either in [[relation]] or in [[other]].
     *
     * @param other the relation to union with [[relation]].
     * @return the union of [[relation]] and [[other]].
     */
    def union(other: R[T]): R[T]

    /**
     * Returns a [[Relation]] with only distinct tuples.
     */
    def distinct(): R[T]

    /**
     * Projects the relation using the given columns.
     *
     * @param columns the columns to project.
     * @return a new relation with the given columns.
     */
    def project(columns: List[Column[T]]): R[T]

    /**
     * Selects the tuples in the relation which match the given selection.
     *
     * @param selection the selection to apply.
     * @return a new relation with the selected tuples.
     */
    def select(selection: Set[Set[Column[T]]]): R[T]
