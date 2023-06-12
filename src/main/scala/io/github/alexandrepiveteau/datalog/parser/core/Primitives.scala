package io.github.alexandrepiveteau.datalog.parser.core

import io.github.alexandrepiveteau.datalog.parser.core.Parser.Result.{Failure, Success}
import io.github.alexandrepiveteau.datalog.parser.core.Parser.{Result, State}

import scala.util.matching.Regex

def token(text: String): Parser[String] = (s: State) =>
  if s.input.startsWith(text, s.from) then Success(text, s.copy(from = s.from + text.length))
  else Failure

def regexToken(text: Regex): Parser[String] = (s: State) =>
  val substring = s.input.substring(s.from)
  text.findFirstMatchIn(substring) match
    case Some(value) if value.start == 0 =>
      val matched = substring.substring(value.start, value.end)
      val newState = s.copy(from = s.from + matched.length)
      Success(matched, newState)
    case _ => Failure

def end(): Parser[Unit] = (s: State) =>
  if s.from == s.input.length then Success((), s)
  else Failure

def all(): Parser[String] = (s: State) =>
  Success(s.input.substring(s.from), s.copy(from = s.input.length))

def optional[A](parser: Parser[A]): Parser[Option[A]] = (s: State) =>
  parser.parse(s) match
    case Success(v, s) => Success(Some(v), s)
    case Failure => Success(None, s)

// PRIMITIVE TYPES

extension (i: Int.type)
  def parser: Parser[Int] = regexToken("-?[0-9]+".r).map(_.toInt)

extension (s: String.type)
  def parserQuoted: Parser[String] = regexToken("\"[^\"]*\"".r)
    .map(_.drop(1).dropRight(1))
