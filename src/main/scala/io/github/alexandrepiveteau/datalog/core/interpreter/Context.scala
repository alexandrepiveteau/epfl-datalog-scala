package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Relation
import io.github.alexandrepiveteau.datalog.core.rule.Value

// TODO : Document this.
case class Context[T](atoms: Iterable[Value[T]], domain: Domain[T])

// TODO : Make this an extension method.
def negated[R[_], T](op: R[T])(using relation: Relation[R], context: Context[T]): R[T] =
  relation.domain(op.arity, context.atoms.toSet)
    .minus(op)
