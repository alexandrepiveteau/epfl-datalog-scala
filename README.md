# epfl-datalog-scala

Datalog evaluation engine for Scala.
Done as part of the CS-498 (Semester Project) course at EPFL.

```scala
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
  val parser = DatalogProgramParser(Int.parser, Int.domain, algorithm = SemiNaive, engine = ExecutionEngine.staged)
  val program = parser.parse(input)
  program.map(_.solve(Predicate("tc"), 2)).toList
    .flatten
    .foreach(fact => println(fact.toString))
}

```

## Download

Coming soon !

## Features

+ Written in uncomplicated Scala
+ Supports various datalog features
    - Naive and semi-naive evaluation
    - Stratified negation
    - Interpreted and multi-staged evaluation
    - Aggregations (`min`, `max`, `sum` and `count`)
+ Parser for text-based datalog programs

## Examples

<details>
<summary>Examples of Datalog programs</summary>
<ul>
<li><a href="./src/test/resources/examples/agg_distinct/program.dl">agg_distinct</a></li>
<li><a href="./src/test/resources/examples/neg/program.dl">neg</a></li>
<li><a href="./src/test/resources/examples/tc/program.dl">tc</a></li>
<li><a href="./src/test/resources/examples/tc_neg/program.dl">tc_neg</a></li>
</ul>
</details>

## Usage

Coming soon !