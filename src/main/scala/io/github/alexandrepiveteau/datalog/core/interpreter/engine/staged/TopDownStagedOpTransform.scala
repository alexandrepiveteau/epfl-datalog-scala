package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.*

/**
 * An implementation of [[StagedOpTransform]] which recursively transforms a [[StagedOp]] into a
 * new [[StagedOp]]. This is a top-down transformation, which means that the transformation is
 * applied to the children of the [[StagedOp]] before being applied to the [[StagedOp]] itself.
 */
abstract class TopDownStagedOpTransform extends StagedOpTransform:

  override def transform[C, O, R[_]](op: StagedOp[C, O, R]): StagedOp[C, O, R] = op match
    case op: SequenceOp[C, R] => transformSequenceOp(op)
    case op: ScanOp[C, R] => transformScanOp(op)
    case op: StoreOp[C, R] => transformStoreOp(op)
    case op: DoWhileNotEqualOp[C, R] => transformDoWhileNotEqualOp(op)
    case op: DoWhileNonEmptyOp[C, R] => transformDoWhileNonEmptyOp(op)
    case op: MergeAndClearOp[C, R] => transformMergeAndClearOp(op)
    case op: EmptyOp[C, R] => transformEmptyOp(op)
    case op: DomainOp[C, R] => transformDomainOp(op)
    case op: JoinOp[C, R] => transformJoinOp(op)
    case op: UnionOp[C, R] => transformUnionOp(op)
    case op: ArityOp[C, R] => transformArityOp(op)
    case op: AggregateOp[C, R] => transformAggregateOp(op)
    case op: MinusOp[C, R] => transformMinusOp(op)
    case op: DistinctOp[C, R] => transformDistinctOp(op)
    case op: ProjectOp[C, R] => transformProjectOp(op)
    case op: SelectOp[C, R] => transformSelectOp(op)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformSequenceOp[C, R[_]](op: SequenceOp[C, R]): StagedOp[C, Unit, R] =
    SequenceOp(op.ops.map(transform))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformScanOp[C, R[_]](op: ScanOp[C, R]): StagedOp[C, R[C], R] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformStoreOp[C, R[_]](op: StoreOp[C, R]): StagedOp[C, Unit, R] =
    StoreOp(op.database, op.predicate, transform(op.relation))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDoWhileNotEqualOp[C, R[_]](op: DoWhileNotEqualOp[C, R]): StagedOp[C, Unit, R] =
    DoWhileNotEqualOp(transform(op.op), op.first, op.second)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDoWhileNonEmptyOp[C, R[_]](op: DoWhileNonEmptyOp[C, R]): StagedOp[C, Unit, R] =
    DoWhileNonEmptyOp(transform(op.op), op.database)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformMergeAndClearOp[C, R[_]](op: MergeAndClearOp[C, R]): StagedOp[C, Unit, R] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformEmptyOp[C, R[_]](op: EmptyOp[C, R]): StagedOp[C, R[C], R] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDomainOp[C, R[_]](op: DomainOp[C, R]): StagedOp[C, R[C], R] = op

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformJoinOp[C, R[_]](op: JoinOp[C, R]): StagedOp[C, R[C], R] =
    JoinOp(op.relations.map(transform))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformUnionOp[C, R[_]](op: UnionOp[C, R]): StagedOp[C, R[C], R] =
    UnionOp(op.relations.map(transform))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformArityOp[C, R[_]](op: ArityOp[C, R]): StagedOp[C, Int, R] =
    ArityOp(transform(op.relation))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformAggregateOp[C, R[_]](op: AggregateOp[C, R]): StagedOp[C, R[C], R] =
    AggregateOp(transform(op.relation), op.projection, op.same, op.aggregate, op.indices)(using op.domain)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformMinusOp[C, R[_]](op: MinusOp[C, R]): StagedOp[C, R[C], R] =
    MinusOp(transform(op.left), transform(op.right))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformDistinctOp[C, R[_]](op: DistinctOp[C, R]): StagedOp[C, R[C], R] =
    DistinctOp(transform(op.relation))

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformProjectOp[C, R[_]](op: ProjectOp[C, R]): StagedOp[C, R[C], R] =
    ProjectOp(transform(op.relation), op.projection)

  /**
   * @see [[transform()]] for the top-level documentation.
   */
  protected def transformSelectOp[C, R[_]](op: SelectOp[C, R]): StagedOp[C, R[C], R] =
    SelectOp(transform(op.relation), op.selection)
