package io.github.alexandrepiveteau.datalog.core.interpreter.engine

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.Algorithm
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.interpreter.InterpreterExecutionEngine
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.staged.StagedExecutionEngine
import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Predicate, Rule}

import scala.quoted.{ToExpr, Type}

/**
 * An [[ExecutionEngine]] can solve Datalog programs. Variants of execution engines, which will either interpret or
 * compile the Datalog program, and which can be implemented by extending this trait.
 *
 * @tparam T the type of the constants in the Datalog program.
 */
trait ExecutionEngine[T]:

  /**
   * Solves a Datalog program, given a predicate, an arity, and an algorithm.
   *
   * @param predicate the predicate to solve.
   * @param arity     the arity of the predicate.
   * @param algorithm the algorithm to use.
   * @param domain    the domain of the Datalog program.
   * @param rules     the rules of the Datalog program.
   * @return the set of facts that were derived for the given predicate and arity.
   */
  def solve(predicate: Predicate,
            arity: Int,
            algorithm: Algorithm)
           (using domain: Domain[T],
            rules: Set[Rule[T]]): Iterable[Fact[T]]

object ExecutionEngine:

  /**
   * An [[ExecutionEngine]] which interprets programs.
   *
   * @tparam T the type of the constants in the Datalog program.
   * @return an [[ExecutionEngine]] which interprets programs.
   */
  def interpreter[T]: ExecutionEngine[T] = InterpreterExecutionEngine()

  // TODO : Support other types.
  inline def staged: ExecutionEngine[Int] = StagedExecutionEngine()
