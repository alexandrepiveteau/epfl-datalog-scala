package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.transforms

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.{StagedOp, TopDownStagedOpTransform, UnionOp}

object FlattenUnion extends TopDownStagedOpTransform:

  override protected def transformUnionOp[C, R[_]](op: UnionOp[C, R]): StagedOp[C, R[C], R] =
    val flattened = op.relations.map(transform)
      .flatMap(op =>
        op match
          case UnionOp(relations) => relations
          case other => Set(other)
      )
    UnionOp(flattened)
