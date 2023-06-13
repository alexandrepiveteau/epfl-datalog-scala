package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.transforms

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.{DistinctOp, StagedOp, StagedOpTransform, TopDownStagedOpTransform}

/**
 * A [[StagedOpTransform]] that eliminates [[DistinctOp]]s from the [[StagedOp]] tree. This can be performed
 * safely because the elements in a [[TupleSet]] are always distinct.
 */
object EliminateDistinct extends TopDownStagedOpTransform:

  override protected def transformDistinctOp[C](op: DistinctOp[C]): StagedOp[C, TupleSet[C]] =
    transform(op.relation)
