package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.transforms

import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.{SequenceOp, StagedOp, TopDownStagedOpTransform}

/**
 * A [[TopDownStagedOpTransform]] which eliminates singleton sequences. This is useful to avoid
 * unnecessary allocations when the [[SequenceOp]] is not needed.
 */
object EliminateSingletonSequence extends TopDownStagedOpTransform:

  override def transformSequenceOp[C](op: SequenceOp[C]): StagedOp[C, Unit] =
    val children = op.ops.map(transform)
    if children.size == 1 then children.head else SequenceOp(children)
