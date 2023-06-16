package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged

import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.StagedOp

/**
 * A transformation which may be applied to the [[StagedOp]]s of a staged engine. The transformation
 * can be used to optimize the execution of the program, by removing the need for some operations and
 * eliminating them completely from the execution.
 */
trait StagedOpTransform:

  /**
   * Transforms the given [[StagedOp]] into another [[StagedOp]], which may be more efficient to
   * execute.
   *
   * @tparam C the type of the constants in the program.
   * @tparam O the type of the output of the operation.
   * @param op the [[StagedOp]] to transform.
   * @return a new [[StagedOp]] which may be more efficient to execute.
   */
  def transform[C, O, R[_]](op: StagedOp[C, O, R]): StagedOp[C, O, R]

object StagedOpTransform:

  /**
   * Optimizes the given [[StagedOp]] by applying the given [[StagedOpTransform]]s until the
   * [[StagedOp]] cannot be optimized anymore.
   *
   * @param op         the [[StagedOp]] to optimize.
   * @param transforms the [[StagedOpTransform]]s to apply.
   * @tparam C the type of the constants in the program.
   * @tparam O the type of the output of the operation.
   * @return a new [[StagedOp]] which may be more efficient to execute.
   */
  def optimize[C, O, R[_]](op: StagedOp[C, O, R], transforms: StagedOpTransform*): StagedOp[C, O, R] =
    val step: StagedOp[C, O, R] => StagedOp[C, O, R] = op =>
      transforms.foldLeft(op)((op, transform) => transform.transform(op))
    var current = op
    while
      val next = step(current)
      val changed = next != current
      current = next
      changed
    do ()
    current

