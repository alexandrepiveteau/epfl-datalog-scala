package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{Database, IROp}

import scala.collection.mutable

// TODO : Document this.
private def dependencies[T](rules: RulesDatabase[T], target: PredicateWithArity): Set[PredicateWithArity] =
  val visited = mutable.Set[PredicateWithArity]()
  val queue = mutable.ArrayDeque.empty[PredicateWithArity]
  queue.addOne(target)
  while queue.nonEmpty do
    val predicate = queue.removeHead()
    if visited.add(predicate) then
      for rule <- rules(predicate) do {
        for clause <- rule.body do {
          queue.addOne(PredicateWithArity(clause.predicate, clause.arity))
        }
      }
  visited.toSet


// TODO : Implement this.
// TODO : Document this.
def stratifiedEval[T](target: PredicateWithArity, idb: RulesDatabase[T],
                      base: Database, result: Database)
                     (evalStrata: (RulesDatabase[T], Database, Database) => IROp[T]): IROp[T] = ???
