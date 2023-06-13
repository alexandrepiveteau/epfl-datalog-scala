package io.github.alexandrepiveteau.datalog.parser

import io.github.alexandrepiveteau.datalog.core.Sum
import io.github.alexandrepiveteau.datalog.core.rule.*
import io.github.alexandrepiveteau.datalog.parser.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldEqual

class ParserTestSuite extends AnyFunSuite:

  test("parses a simple rule with one clause correctly") {
    val rule = "a(X):-b(X)."
    val expected = CombinationRule[Int](
      head = HeadLiteral(Predicate("a"), List(Variable("X"))),
      body = List(BodyLiteral(Predicate("b"), List(Variable("X")), false)),
    )

    val parser = for
      r <- DatalogRuleParser(Int.parser)
      _ <- end()
    yield r

    parser.parse(rule) shouldEqual Some(expected)
  }

  test("parses a rule with aggregation correctly") {
    val rule = "a(x,s):-b(x,v),sum((x),(v),s)."
    val expected =
      AggregationRule(
        head = HeadLiteral(Predicate("a"), List(Variable("x"), Variable("s"))),
        clause = BodyLiteral(Predicate("b"), List(Variable("x"), Variable("v")), false),
        aggregate =
          Aggregate(
            agg = Sum,
            same = List(Variable("x")),
            columns = List(Variable("v")),
            result = Variable("s"),
          ),
      )

    val parser = for
      r <- DatalogRuleParser(Int.parser)
      _ <- end()
    yield r
    parser.parse(rule) shouldEqual Some(expected)
  }
