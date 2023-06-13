package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.*

/**
 * An implementation of [[StagedOpTransform]] which recursively transforms a [[StagedOp]] into a
 * new [[StagedOp]]. This is a top-down transformation, which means that the transformation is
 * applied to the children of the [[StagedOp]] before being applied to the [[StagedOp]] itself.
 */
abstract class TopDownStagedOpTransform extends StagedOpTransform:

  override def transform[C, O](op: StagedOp[C, O]): StagedOp[C, O] = op match
    case op: SequenceOp[C] => transformSequenceOp(op)
    case op: ScanOp[C] => transformScanOp(op)
    case op: StoreOp[C] => transformStoreOp(op)
    case op: DoWhileNotEqualOp[C] => transformDoWhileNotEqualOp(op)
    case op: DoWhileNonEmptyOp[C] => transformDoWhileNonEmptyOp(op)
    case op: MergeAndClearOp[C] => transformMergeAndClearOp(op)
    case op: EmptyOp[C] => transformEmptyOp(op)
    case op: DomainOp[C] => transformDomainOp(op)
    case op: JoinOp[C] => transformJoinOp(op)
    case op: UnionOp[C] => transformUnionOp(op)
    case op: ArityOp[C] => transformArityOp(op)
    case op: AggregateOp[C] => transformAggregateOp(op)
    case op: MinusOp[C] => transformMinusOp(op)
    case op: DistinctOp[C] => transformDistinctOp(op)
    case op: ProjectOp[C] => transformProjectOp(op)
    case op: SelectOp[C] => transformSelectOp(op)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformSequenceOp[C](op: SequenceOp[C]): StagedOp[C, Unit] =
    SequenceOp(op.ops.map(transform))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformScanOp[C](op: ScanOp[C]): StagedOp[C, TupleSet[C]] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformStoreOp[C](op: StoreOp[C]): StagedOp[C, Unit] =
    StoreOp(op.database, op.predicate, transform(op.relation))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDoWhileNotEqualOp[C](op: DoWhileNotEqualOp[C]): StagedOp[C, Unit] =
    DoWhileNotEqualOp(transform(op.op), op.first, op.second)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDoWhileNonEmptyOp[C](op: DoWhileNonEmptyOp[C]): StagedOp[C, Unit] =
    DoWhileNonEmptyOp(transform(op.op), op.database)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformMergeAndClearOp[C](op: MergeAndClearOp[C]): StagedOp[C, Unit] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformEmptyOp[C](op: EmptyOp[C]): StagedOp[C, TupleSet[C]] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDomainOp[C](op: DomainOp[C]): StagedOp[C, TupleSet[C]] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformJoinOp[C](op: JoinOp[C]): StagedOp[C, TupleSet[C]] =
    JoinOp(op.relations.map(transform))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformUnionOp[C](op: UnionOp[C]): StagedOp[C, TupleSet[C]] =
    UnionOp(op.relations.map(transform))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformArityOp[C](op: ArityOp[C]): StagedOp[C, Int] =
    ArityOp(transform(op.relation))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformAggregateOp[C](op: AggregateOp[C]): StagedOp[C, TupleSet[C]] =
    AggregateOp(transform(op.relation), op.projection, op.same, op.aggregate, op.indices)(using op.domain)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformMinusOp[C](op: MinusOp[C]): StagedOp[C, TupleSet[C]] =
    MinusOp(transform(op.left), transform(op.right))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDistinctOp[C](op: DistinctOp[C]): StagedOp[C, TupleSet[C]] =
    DistinctOp(transform(op.relation))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformProjectOp[C](op: ProjectOp[C]): StagedOp[C, TupleSet[C]] =
    ProjectOp(transform(op.relation), op.projection)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformSelectOp[C](op: SelectOp[C]): StagedOp[C, TupleSet[C]] =
    SelectOp(transform(op.relation), op.selection)
