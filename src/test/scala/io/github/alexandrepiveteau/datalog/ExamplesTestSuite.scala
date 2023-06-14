package io.github.alexandrepiveteau.datalog

import io.github.alexandrepiveteau.datalog.core.interpreter.engine.ExecutionEngine
import io.github.alexandrepiveteau.datalog.core.interpreter.{Algorithm, Naive, SemiNaive}
import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Predicate, Rule, Value}
import io.github.alexandrepiveteau.datalog.core.{ProgramBuilder, domain}
import io.github.alexandrepiveteau.datalog.parser.DatalogRuleParser
import io.github.alexandrepiveteau.datalog.parser.core.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldEqual

import java.io.*
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors
import scala.jdk.StreamConverters.*

/**
 * An [[AnyFunSuite]] which will run all the examples in the `examples` resource folder. Each
 * example is composed of a Datalog program, some `input` relations, and some `output` relations.
 */
class ExamplesTestSuite extends AnyFunSuite:
  for example <- examples() do
    for data <- cases(example) do
      for (algorithmName, algorithm) <- algorithms() do
        for (engineName, engine) <- engines() do
          test(s"${example.getName}/${data.getName}/$engineName/$algorithmName") {
            testCase(example, data, engine, algorithm)
          }

// File and folder names.
private inline val ExamplesFolder = "examples"
private inline val CasesFolder = "cases"
private inline val InputFolder = "input"
private inline val OutputFolder = "output"
private inline val ProgramFile = "program.dl"
private inline val ConfigFile = "config.dl"

/**
 * Returns a [[List]] with all the example files.
 */
private def examples(): List[File] =
  val classLoader = Thread.currentThread().getContextClassLoader
  val path = classLoader.getResource(ExamplesFolder).getPath
  File(path).listFiles().toList

/**
 * Returns all the cases in the given [[folder]].
 */
private def cases(folder: File): List[File] =
  File(folder, CasesFolder).listFiles().toList

/**
 * Returns all the algorithms that can be used.
 */
private def algorithms(): List[(String, Algorithm)] =
  List(
    "naive" -> Naive,
    "semi-naive" -> SemiNaive,
  )

/**
 * Returns all the execution engines.
 */
private def engines(): List[(String, ExecutionEngine[Int])] =
  List(
    "interpreter" -> ExecutionEngine.interpreter[Int],
    "staged" -> ExecutionEngine.staged,
  )

/**
 * Runs a test case.
 */
private def testCase(program: File, data: File, engine: ExecutionEngine[Int], algorithm: Algorithm): Unit =
  val inputs = File(data, InputFolder).listFiles().toList
  val outputs = File(data, OutputFolder).listFiles().toList
  val programFile = File(program, ProgramFile)

  val ruleList = rules(programFile)

  // 1. Prepare the program.
  val builder = ProgramBuilder[Int](Int.domain, algorithm)
  for rule <- ruleList do builder.rule(rule)
  for file <- inputs do
    println(file)
    println(file.getName)
    val relation = file.getName.split("\\.").head
    val predicate = Predicate(relation)
    facts(file).foreach(builder.fact(predicate, _))

  // 2. Run the program for each tested relation.
  val datalog = builder.build(engine)
  for file <- outputs do
    val splits = file.getName.split("\\.")
    val predicate = Predicate(splits.head)
    val arity = splits(1).toInt
    val actual = datalog.solve(predicate, arity).toSet
    val expected = facts(file).toSet
    actual shouldEqual expected


/**
 * Returns all the [[Rule]]s from a file.
 */
private def rules(program: File): List[Rule[Int]] =
  val ruleParser = DatalogRuleParser(Int.parser)
  val ws = regexToken("\\s*".r)
  val text = program.readText()
  val parser = for
    l <- separated(ruleParser, ws)
    _ <- end()
  yield l
  parser.parse(text).getOrElse(List.empty)

/**
 * Returns all the [[Fact]]s from a file.
 */
private def facts(file: File): List[Fact[Int]] =
  val constants = Int.parser
  val lines = file.readLines()
  lines.map(it =>
    it.split(",").map(v => Value(constants.parse(v).get)).toList
  )

// Standard Java I/O extensions. Inspired from the Kotlin stdlib.

extension (file: File)

  def readLines(): List[String] =
    val stream = FileInputStream(file)
    val reader = InputStreamReader(stream, StandardCharsets.UTF_8)
    val res = reader.lines()
    reader.close()
    res

  def readText(): String =
    val stream = FileInputStream(file)
    val reader = InputStreamReader(stream, StandardCharsets.UTF_8)
    val buffer = StringWriter()
    reader.copyTo(buffer)
    reader.close()
    buffer.toString

private inline val DefaultBufferSize = 8 * 1024

extension (reader: Reader)

  def lines(): List[String] =
    val buffered = reader.buffered()
    buffered.lines().toScala(List)

  def buffered(bufferSize: Int = DefaultBufferSize): BufferedReader =
    reader match
      case reader: BufferedReader => reader
      case _ => BufferedReader(reader, bufferSize)

  def copyTo(out: Writer, bufferSize: Int = DefaultBufferSize): Long =
    var charsCopied: Long = 0
    val buffer = Array.ofDim[Char](bufferSize)
    var chars = reader.read(buffer)
    while chars >= 0 do
      out.write(buffer, 0, chars)
      charsCopied += chars
      chars = reader.read(buffer)
    charsCopied
