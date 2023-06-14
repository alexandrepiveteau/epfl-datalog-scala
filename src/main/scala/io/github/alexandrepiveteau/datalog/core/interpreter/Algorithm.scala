package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.database.RulesDatabase
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{Database, IROp}

// TODO : Document this.
sealed trait Algorithm {
  // TODO : Document this.
  def evaluate[O[_, _], R[_], T: Context](idb: RulesDatabase[T], base: Database, result: Database)
                                         (using op: IROp[O, R]): O[T, Unit]
}

// TODO : Document this.
object Naive extends Algorithm {
  override def evaluate[O[_, _], R[_], T: Context](idb: RulesDatabase[T], base: Database, result: Database)
                                                  (using op: IROp[O, R]): O[T, Unit] =
    naiveEval(idb, base, result)
}

// TODO : Document this.
object SemiNaive extends Algorithm {
  override def evaluate[O[_, _], R[_], T: Context](idb: RulesDatabase[T], base: Database, result: Database)
                                                  (using op: IROp[O, R]): O[T, Unit] =
    semiNaiveEval(idb, base, result)
}
