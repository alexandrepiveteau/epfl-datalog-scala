package io.github.alexandrepiveteau.datalog.core.interpreter.database

import io.github.alexandrepiveteau.datalog.core.rule.Predicate

// TODO : Document this.
case class PredicateWithArity(predicate: Predicate, arity: Int)
