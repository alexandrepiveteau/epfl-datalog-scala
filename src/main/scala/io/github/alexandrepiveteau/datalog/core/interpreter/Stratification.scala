package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{Database, IROp}

// TODO : Implement this.
// TODO : Document this.
def stratifiedEval[T](target: PredicateWithArity, idb: RulesDatabase[T],
                      base: Database, result: Database)
                     (evalStrata: (RulesDatabase[T], Database, Database) => IROp[T]): IROp[T] = ???
