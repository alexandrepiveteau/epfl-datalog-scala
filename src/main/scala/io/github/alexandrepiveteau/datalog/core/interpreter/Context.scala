package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{IRRelDomain, IRRelMinus, IRRelOp}
import io.github.alexandrepiveteau.datalog.core.rule.Value

// TODO : Document this.
case class Context[T](atoms: Iterable[Value[T]], domain: Domain[T])

// TODO : Document this.
extension[T] (op: IRRelOp[T])

  // TODO : Document this.
  def negated(using context: Context[T]): IRRelOp[T] =
    IRRelMinus(IRRelDomain(op.arity, context.atoms), op)
