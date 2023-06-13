package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.{TupleSet, given}
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{FactsDatabase, PredicateWithArity, RulesDatabase, StorageManager}
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.interpreter.{IROpInterpreter, given}
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.{ContextExecutionEngine, ExecutionEngine}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Database.{Base, Result}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{*, given}
import io.github.alexandrepiveteau.datalog.core.rule.*
import io.github.alexandrepiveteau.datalog.core.{Domain, Program}

import scala.collection.mutable

// TODO : Document this.
class DatalogProgram[T](val domain: Domain[T],
                        val rules: Set[Rule[T]],
                        val algorithm: Algorithm,
                        val engine: ExecutionEngine[T]) extends Program[T]:

  override def solve(predicate: Predicate, arity: Int): Iterable[Fact[T]] =
    engine.solve(predicate, arity, algorithm)(using domain, rules)
