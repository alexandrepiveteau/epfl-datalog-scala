package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.database.*
import io.github.alexandrepiveteau.datalog.core.rule.*

import scala.collection.mutable

// TODO : Document this.
private def requireFact[T](rule: CombinationRule[T]): Fact[T] =
  if (rule.body.nonEmpty) throw new IllegalArgumentException("Rule is not be a fact.")
  val result = rule.head.terms.filter(it => it.isInstanceOf[Value[T]])
  val mapped = result.map(it => it.asInstanceOf[Value[T]])
  if (mapped.size != rule.head.arity) throw new IllegalArgumentException("This fact has some unsafe variables.")
  mapped

// TODO : Document this.
def partition[T](rules: Iterable[Rule[T]]): (RulesDatabase[T], FactsDatabase[T]) =
  val edbBuilder = MutableFactsDatabase.builder[T]()
  val idbBuilder = mutable.Map[PredicateWithArity, mutable.Set[Rule[T]]]()
  rules.foreach {
    case rule@(combination@CombinationRule(head, body)) =>
      val predicate = PredicateWithArity(head.predicate, head.arity)
      if body.isEmpty then
        edbBuilder.add(predicate, requireFact(combination))
      else
        idbBuilder.getOrElseUpdate(predicate, mutable.Set.empty) += rule
    case rule@AggregationRule(head, _, _) =>
      val predicate = PredicateWithArity(head.predicate, head.arity)
      idbBuilder.getOrElseUpdate(predicate, mutable.Set.empty) += rule
  }
  (MapRulesDatabase(idbBuilder.map((k, v) => (k, v.toSet)).toMap), edbBuilder.build())
