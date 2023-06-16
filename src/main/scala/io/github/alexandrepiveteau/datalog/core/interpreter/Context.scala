package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.IROp
import io.github.alexandrepiveteau.datalog.core.rule.Value

// TODO : Document this.
case class Context[T](terms: Iterable[Value[T]], domain: Domain[T])

// TODO : Make this an extension method.
def negated[C, O[_], R[_]](arity: Int, relation: O[R[C]])(using op: IROp[C, O, R], context: Context[C]): O[R[C]] =
  op.domain(arity, context.terms.toSet).minus(relation)
