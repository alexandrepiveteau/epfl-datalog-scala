package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.transforms

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.{SelectOp, StagedOp, TopDownStagedOpTransform}

/**
 * A [[TopDownStagedOpTransform]] which optimizes the selection clauses from a [[SelectOp]].
 */
object OptimizeSelect extends TopDownStagedOpTransform:

  override def transformSelectOp[C, R[_]](op: SelectOp[C, R]): StagedOp[C, R[C], R] =
    val child = transform(op.relation)
    val filtered = op.selection.filter(_.size > 1)
    if filtered.isEmpty then child
    else SelectOp(child, filtered)
