package io.github.alexandrepiveteau.datalog.parser

import io.github.alexandrepiveteau.datalog.core.interpreter.{Algorithm, Naive}
import io.github.alexandrepiveteau.datalog.core.{Domain, Program, ProgramBuilder}
import io.github.alexandrepiveteau.datalog.parser.core.*
import io.github.alexandrepiveteau.datalog.parser.core.Parser.Result.Success
import io.github.alexandrepiveteau.datalog.parser.core.Parser.{Result, State}

/**
 * An implementation of [[Parser]] for Datalog programs.
 *
 * @param constant  the parser for the constant type used in the rules.
 * @param domain    the domain of the program.
 * @param algorithm the algorithm used to evaluate the program.
 * @tparam T the type of the constant.
 */
case class DatalogProgramParser[T](constant: Parser[T], domain: Domain[T], algorithm: Algorithm = Naive) extends Parser[Program[T]]:

  // Whitespace.
  private val ws = regexToken("\\s*".r)

  // Rules.
  private val rule = DatalogRuleParser(constant)
  private val rules = separated(rule, ws)

  // Program.
  private val program = rules.map(rules =>
    val builder = ProgramBuilder(domain, algorithm)
    for rule <- rules do builder.rule(rule)
    builder.build()
  )

  // Program.
  override def parse(state: State): Result[Program[T]] = program.parse(state)
