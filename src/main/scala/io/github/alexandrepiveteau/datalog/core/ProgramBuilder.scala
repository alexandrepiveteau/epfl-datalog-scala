package io.github.alexandrepiveteau.datalog.core

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
  def rule(predicate: Predicate, atoms: List[Atom[T]], block: RuleBuilder[T] ?=> Unit): Unit

  // TODO : Document this.
  def rule(rule: Rule[T]): Unit

  // TODO : Document this.
  def build(): Program[T]
