package io.github.alexandrepiveteau.datalog.core.interpreter.ir

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.{AggregationColumn, Column, Index}
import io.github.alexandrepiveteau.datalog.core.interpreter.database.PredicateWithArity
import io.github.alexandrepiveteau.datalog.core.rule.Value
import io.github.alexandrepiveteau.datalog.core.{AggregationFunction, Domain}

// TODO : Document this.
sealed trait IROp[+T]

// TODO : Document this.
case class IRSequence[+T](ops: List[IROp[T]]) extends IROp[T]

// TODO : Document this.
case class IRStore[+T](database: Database, predicate: PredicateWithArity, relation: IRRelOp[T]) extends IROp[T]

// TODO : Document this.
case class IRDoWhileNotEqual[+T](op: IROp[T], first: Database, second: Database) extends IROp[T]

// TODO : Document this.
case class IRDoWhileNotEmpty[+T](op: IROp[T], database: Database) extends IROp[T]

// TODO : Document this.
case object IRMergeAndClear extends IROp[Nothing]

// TODO : Document this.
sealed trait IRRelOp[+T] extends IROp[T]:
  val arity: Int

// TODO : Document this.
case class IRRelEmpty[+T](arity: Int) extends IRRelOp[T]

// TODO : Document this.
case class IRRelDomain[+T](arity: Int, values: Iterable[Value[T]]) extends IRRelOp[T]

// TODO : Document this.
case class IRRelScan[+T](database: Database, predicate: PredicateWithArity) extends IRRelOp[T]:
  override val arity: Int = predicate.arity

// TODO : Document this.
case class IRRelSelect[T](relation: IRRelOp[T], selection: Set[Set[Column[T]]]) extends IRRelOp[T]:
  override val arity: Int = relation.arity

// TODO : Document this.
case class IRRelJoin[+T](relations: List[IRRelOp[T]]) extends IRRelOp[T]:
  override val arity: Int = relations.map(_.arity).sum

// TODO : Document this.
case class IRRelUnion[+T](first: IRRelOp[T], second: IRRelOp[T]) extends IRRelOp[T]:
  override val arity: Int = first.arity

// TODO : Document this.
case class IRRelDistinct[+T](relation: IRRelOp[T]) extends IRRelOp[T]:
  override val arity: Int = relation.arity

// TODO : Document this.
case class IRRelMinus[+T](relation: IRRelOp[T], removed: IRRelOp[T]) extends IRRelOp[T]:
  override val arity: Int = relation.arity

// TODO : Document this.
case class IRRelProject[+T](relation: IRRelOp[T], columns: List[Column[T]]) extends IRRelOp[T]:
  override val arity: Int = columns.size

// TODO : Document this.
case class IRRelAggregate[T](relation: IRRelOp[T],
                             projection: List[AggregationColumn[T]],
                             same: Set[Index],
                             domain: Domain[T],
                             aggregate: AggregationFunction,
                             indices: Set[Index],
                            ) extends IRRelOp[T]:
  override val arity: Int = projection.size
