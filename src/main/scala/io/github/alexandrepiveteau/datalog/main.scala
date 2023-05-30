package io.github.alexandrepiveteau.datalog

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.Relation
import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.union

@main
def main(): Unit = {
  Relation.empty[Int](123).union(Relation.empty[Int](123))
  println("Hello world!")
}
