package io.github.alexandrepiveteau.datalog.core

import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Predicate}

// TODO : Document this.
trait Program[+T]:

  // TODO : Document this.
  def solve(predicate: Predicate, arity: Int): Iterable[Fact[T]]
