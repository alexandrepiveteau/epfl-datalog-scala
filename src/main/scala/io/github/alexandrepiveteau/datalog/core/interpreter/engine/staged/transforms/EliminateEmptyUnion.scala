package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.transforms

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.*

/**
 * A [[TopDownStagedOpTransform]] that eliminates [[UnionOp]]s that have an [[EmptyOp]] as one of
 * their children.
 */
object EliminateEmptyUnion extends TopDownStagedOpTransform:

  override def transformUnionOp[C](op: UnionOp[C]): StagedOp[C, TupleSet[C]] =
    if op.left.isInstanceOf[EmptyOp[C]] then transform(op.right)
    else if op.right.isInstanceOf[EmptyOp[C]] then transform(op.left)
    else UnionOp(transform(op.left), transform(op.right))
