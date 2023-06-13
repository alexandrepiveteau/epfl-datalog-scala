package io.github.alexandrepiveteau.datalog.core.interpreter.engine.interpreter

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase, StorageManager}
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.ContextExecutionEngine
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Database.{Base, Result}
import io.github.alexandrepiveteau.datalog.core.interpreter.{Algorithm, Context, stratifiedEval}

class InterpreterExecutionEngine[T] extends ContextExecutionEngine[T]:

  override def evaluate(storage: StorageManager[T],
                        target: PredicateWithArity,
                        idb: RulesDatabase[T],
                        algorithm: Algorithm,
                        domain: Domain[T],
                        context: Context[T]): Unit =
    given Context[T] = context

    val eval = stratifiedEval[IROpInterpreter, TupleSet, T](target, idb, Base, Result)((i, e, r) => algorithm.evaluate(i, e, r))
    eval(storage) 
