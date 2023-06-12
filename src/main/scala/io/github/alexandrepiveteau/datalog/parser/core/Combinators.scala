package io.github.alexandrepiveteau.datalog.parser.core

import io.github.alexandrepiveteau.datalog.parser.core.Parser.Result.{Failure, Success}
import io.github.alexandrepiveteau.datalog.parser.core.Parser.{Result, State}

import scala.annotation.targetName
import scala.collection.immutable.List
import scala.util.control.Breaks.{break, breakable}

def separated[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = (s: State) =>
  val list = List.newBuilder[A]
  var currentState = s
  breakable {
    while true do
      parser.parse(currentState) match
        case Failure => break()
        case Success(a, s) =>
          list += a
          currentState = s
      separator.parse(currentState) match
        case Failure => break()
        case Success(_, s) => currentState = s
  }
  Success(list.result(), currentState)

extension[A] (p: Parser[A])

  def map[B](f: A => B): Parser[B] = (s: State) =>
    p.parse(s) match
      case Success(a, s1) => Result.Success(f(a), s1)
      case Failure => Failure

  def flatMap[B](f: A => Parser[B]): Parser[B] = (s: State) =>
    p.parse(s) match
      case Success(a, s1) => f(a).parse(s1)
      case Failure => Failure

  @targetName("or")
  def |[B >: A](q: Parser[B]): Parser[B] = (s: State) =>
    p.parse(s) match
      case s: Success[A] => s
      case Failure => q.parse(s)
