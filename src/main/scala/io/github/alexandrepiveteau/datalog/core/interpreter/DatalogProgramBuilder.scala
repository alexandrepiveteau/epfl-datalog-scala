package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.*
import io.github.alexandrepiveteau.datalog.core.interpreter.engine.ExecutionEngine
import io.github.alexandrepiveteau.datalog.core.rule.*

import scala.collection.mutable

// TODO : Document this.
class DatalogProgramBuilder[T](private val domain: Domain[T],
                               private val algorithm: Algorithm) extends ProgramBuilder[T]:

  // TODO : Document this.
  private var nextRelation = 0
  // TODO : Document this.
  private var nextVariable = 0

  override def predicate(): Predicate =
    nextRelation += 1
    Predicate(nextRelation.toString)

  override def variable(): Variable =
    nextVariable += 1
    Variable(nextVariable.toString)

  // TODO : Document this.
  private val rules = mutable.Set[Rule[T]]()

  // TODO : Document this.
  private def limited(rule: Rule[T]): Set[Variable] =
    val variables = mutable.Set[Variable]()
    rule.body
      .filter(it => !it.negated)
      .foreach { clause =>
        val filtered = clause.atoms
          .filter(a => a.isInstanceOf[Variable])
          .map(a => a.asInstanceOf[Variable])
        variables.addAll(filtered)
      }
    rule match
      case _: CombinationRule[T] => ()
      case AggregationRule(_, _, aggregate) => variables.add(aggregate.result)
    variables.toSet

  // TODO : Document this.
  private def requireGrounding(rule: Rule[T]): Unit =
    val head = rule.head.atoms.filter(_.isInstanceOf[Variable]).map(_.asInstanceOf[Variable])
    val body = rule.body.flatMap(_.atoms).filter(_.isInstanceOf[Variable]).map(_.asInstanceOf[Variable])
    val l = limited(rule)
    for variable <- head ++ body do
      if !l.contains(variable) then throw NotGroundedException()

  override def rule(predicate: Predicate, atoms: List[Atom[T]], block: RuleBuilder[T] ?=> Unit): Unit =
    val context = DatalogRuleBuilder[T]
    block(using context)
    rule(context.toRule(predicate, atoms))

  override def rule(rule: Rule[T]): Unit =
    requireGrounding(rule)
    rules.add(rule)

  override def build(engine: ExecutionEngine[T]): Program[T] = new DatalogProgram(domain, rules.toSet, algorithm, engine)

