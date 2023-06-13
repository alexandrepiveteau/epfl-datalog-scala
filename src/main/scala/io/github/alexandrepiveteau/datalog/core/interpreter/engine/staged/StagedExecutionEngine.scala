package io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase, StorageManager}
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.ContextExecutionEngine
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Database.{Base, Result}
import io.github.alexandrepiveteau.datalog.core.interpreter.{Algorithm, Context, stratifiedEval}

import scala.quoted.*

class StagedExecutionEngine extends ContextExecutionEngine[Int]:

  given ToExprUnit: ToExpr[Unit] with
    def apply(u: Unit)(using Quotes): Expr[Unit] = '{}

  override def evaluate(storage: StorageManager[Int],
                        target: PredicateWithArity,
                        idb: RulesDatabase[Int],
                        algorithm: Algorithm,
                        domain: Domain[Int],
                        context: Context[Int]): Unit =
    given Context[Int] = context

    val ir = stratifiedEval[StagedOp, TupleSet, Int](target, idb, Base, Result)((i, e, r) => algorithm.evaluate(i, e, r))

    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    val compiled: (StorageManager[Int], Domain[Int]) => Unit = staging.run {
      val expr = '{ (s: StorageManager[Int], d: Domain[Int]) =>
        ${
          given Expr[StorageManager[Int]] = 's
          given Expr[Domain[Int]] = 'd
          compile(ir)
        }
      }
      println(expr.show)
      expr
    }

    compiled(storage, domain)

