package io.github.alexandrepiveteau.datalog

import io.github.alexandrepiveteau.datalog.core.domain
import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.TupleSet
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.ExecutionEngine
import io.github.alexandrepiveteau.datalog.core.rule.Predicate
import io.github.alexandrepiveteau.datalog.parser.DatalogProgramParser
import io.github.alexandrepiveteau.datalog.parser.core.{parse, parser}

@main
def main(): Unit = {
  val input =
    """
      |e(1,2).
      |e(2,3).
      |e(3,4).
      |e(4,5).
      |tc(X,Y):-e(X, Y).
      |tc(X,Y):-e(X, Z),tc(Z, Y).
      |""".stripMargin
  val parser = DatalogProgramParser(Int.parser, Int.domain, engine=ExecutionEngine.staged)
  val program = parser.parse(input)
  program.map(_.solve(Predicate("tc"), 2)).toList
    .flatten
    .foreach(fact => println(fact.toString))
}
