package io.github.alexandrepiveteau.datalog.parser

import io.github.alexandrepiveteau.datalog.core.rule.*
import io.github.alexandrepiveteau.datalog.core.{Count, Max, Min, Sum}
import io.github.alexandrepiveteau.datalog.parser.core.*

import scala.util.matching.Regex

/**
 * An implementation of [[Parser]] for Datalog rules.
 *
 * @param constant the parser for the constant type used in the rules.
 * @tparam T the type of the constant used in the rules.
 */
case class DatalogRuleParser[+T](constant: Parser[T]) extends Parser[Rule[T]]:

  // Tokens and keywords.
  private val lpar = token("(")
  private val rpar = token(")")
  private val comma = token(",")
  private val dot = token(".")
  private val implies = token(":-")
  private val not = token("!")

  // Aggregate names.
  private val min = token("min") map { _ => Min }
  private val max = token("max") map { _ => Max }
  private val sum = token("sum") map { _ => Sum }
  private val count = token("count") map { _ => Count }

  // Names.
  private val ws = regexToken("\\s*".r)
  private val name = for
    _ <- ws
    token <- regexToken("[a-zA-Z_][a-zA-Z0-9_]*".r)
    _ <- ws
  yield token

  // Datalog-specific tokens.
  private val predicate = name map Predicate.apply
  private val variable = name map Variable.apply
  private val variables: Parser[List[Variable]] = separated(variable, comma)
  private val value = constant map Value.apply
  private val atom = variable | value
  private val atoms: Parser[List[Atom[T]]] = separated(atom, comma)
  private val literal = for
    p <- predicate
    _ <- lpar
    a <- atoms
    _ <- rpar
  yield (p, a)
  private val headLiteral = literal.map(HeadLiteral.apply)
  private val bodyLiteral = for
    neg <- optional(not) map (_.isDefined)
    lit <- literal
  yield BodyLiteral(lit._1, lit._2, neg)
  private val bodyLiterals: Parser[List[BodyLiteral[T]]] = separated(bodyLiteral, comma)
  private val aggregate = for
    agg <- min | max | sum | count
    _ <- lpar
    _ <- lpar
    vars <- variables // same
    _ <- rpar
    _ <- comma
    _ <- lpar
    cols <- variables // columns
    _ <- rpar
    _ <- comma
    res <- variable // result
    _ <- rpar
  yield Aggregate(agg, vars, cols, res)

  // Rules.
  private val aggregationRule = for
    head <- headLiteral
    _ <- implies
    body <- bodyLiteral
    _ <- comma
    agg <- aggregate
    _ <- dot
  yield AggregationRule(head, body, agg)
  private val combinationRule = for
    head <- headLiteral
    _ <- implies
    body <- bodyLiterals
    _ <- dot
  yield CombinationRule(head, body)
  private val fact = for
    head <- headLiteral
    _ <- dot
  yield CombinationRule(head, Nil)

  private val rule = aggregationRule | combinationRule | fact

  override def parse(state: Parser.State): Parser.Result[Rule[T]] = rule.parse(state)
