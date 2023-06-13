package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{IROp, Relation}
import io.github.alexandrepiveteau.datalog.core.rule.Value

// TODO : Document this.
case class Context[T](terms: Iterable[Value[T]], domain: Domain[T])

// TODO : Make this an extension method.
def negated[O[_, _], R[_], T](arity: Int, relation: O[T, R[T]])(using op: IROp[O, R], context: Context[T]): O[T, R[T]] =
  op.domain[T](arity, context.terms.toSet).minus(relation)
