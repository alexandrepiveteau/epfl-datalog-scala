package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.database.RulesDatabase
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{Database, IROp}

// TODO : Document this.
sealed trait Algorithm {
  // TODO : Document this.
  def evaluate[C: Context, O[_], R[_]](idb: RulesDatabase[C], base: Database, result: Database)
                                      (using op: IROp[C, O, R]): O[Unit]
}

// TODO : Document this.
object Naive extends Algorithm {
  def evaluate[C: Context, O[_], R[_]](idb: RulesDatabase[C], base: Database, result: Database)
                                      (using op: IROp[C, O, R]): O[Unit] =
    naiveEval(idb, base, result)
}

// TODO : Document this.
object SemiNaive extends Algorithm {
  def evaluate[C: Context, O[_], R[_]](idb: RulesDatabase[C], base: Database, result: Database)
                                      (using op: IROp[C, O, R]): O[Unit] =
    semiNaiveEval(idb, base, result)
}
