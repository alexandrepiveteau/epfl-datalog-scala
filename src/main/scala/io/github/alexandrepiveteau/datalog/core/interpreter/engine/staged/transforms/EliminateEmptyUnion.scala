package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.transforms

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.*

/**
 * A [[TopDownStagedOpTransform]] that eliminates [[UnionOp]]s that have an [[EmptyOp]] as one of
 * their children.
 */
object EliminateEmptyUnion extends TopDownStagedOpTransform:

  override def transformUnionOp[C](op: UnionOp[C]): StagedOp[C, TupleSet[C]] =
    val filtered = op.relations.map(transform)
      .filter(!_.isInstanceOf[EmptyOp[C]])
    UnionOp(filtered)
