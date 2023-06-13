package io.github.alexandrepiveteau.datalog.core

import io.github.alexandrepiveteau.datalog.core.interpreter.engine.ExecutionEngine
import io.github.alexandrepiveteau.datalog.core.interpreter.{Algorithm, DatalogProgramBuilder, Naive}
import io.github.alexandrepiveteau.datalog.core.rule.*

// TODO : Document this.
trait ProgramBuilder[T]:

  // TODO : Document this.
  def predicate(): Predicate

  // TODO : Document this.
  def variable(): Variable

  // TODO : Document this.
  def fact(predicate: Predicate, fact: Fact[T]): Unit = rule(predicate, fact, ())

  // TODO : Document this.
  def rule(predicate: Predicate, terms: List[Term[T]], block: RuleBuilder[T] ?=> Unit): Unit

  // TODO : Document this.
  def rule(rule: Rule[T]): Unit

  // TODO : Document this.
  def build(engine: ExecutionEngine[T] = ExecutionEngine.interpreter): Program[T]

// TODO : Document this.
object ProgramBuilder:

  // TODO : Document this.
  def apply[T](domain: Domain[T], algorithm: Algorithm = Naive): ProgramBuilder[T] =
    DatalogProgramBuilder(domain, algorithm)