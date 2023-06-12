package io.github.alexandrepiveteau.datalog.parser.core

import io.github.alexandrepiveteau.datalog.parser.core.Parser.Result.{Failure, Success}
import io.github.alexandrepiveteau.datalog.parser.core.Parser.State

trait Parser[+O]:

  def parse(state: Parser.State): Parser.Result[O]

object Parser:

  case class State(input: String, from: Int)

  sealed trait Result[+O]

  object Result:

    case class Success[+O](output: O, state: State) extends Result[O]

    case object Failure extends Result[Nothing]

extension[O] (parser: Parser[O])

  def parse(input: String): Option[O] =
    val state = State(input, 0)
    parser.parse(state) match
      case Success(value, _) => Some(value)
      case Failure => None
