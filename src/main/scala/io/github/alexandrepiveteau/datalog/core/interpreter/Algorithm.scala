package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.database.RulesDatabase
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{Database, IROp}

// TODO : Document this.
sealed trait Algorithm {
  // TODO : Document this.
  def evaluate[T](rules: RulesDatabase[T], base: Database, result: Database)
                 (using context: Context[T]): IROp[T]
}

// TODO : Document this.
object Naive extends Algorithm {
  override def evaluate[T](rules: RulesDatabase[T], base: Database, result: Database)
                          (using context: Context[T]): IROp[T] =
    naiveEval(rules, base, result)
}

// TODO : Document this.
object SemiNaive extends Algorithm {
  override def evaluate[T](rules: RulesDatabase[T], base: Database, result: Database)
                          (using context: Context[T]): IROp[T] =
    semiNaiveEval(rules, base, result)
}
