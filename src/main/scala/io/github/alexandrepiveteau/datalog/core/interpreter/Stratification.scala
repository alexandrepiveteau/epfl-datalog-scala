package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.NoStratificationException
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{PredicateWithArity, RulesDatabase}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.*
import io.github.alexandrepiveteau.datalog.core.rule.{AggregationRule, CombinationRule, Rule}
import io.github.alexandrepiveteau.graphs.algorithms.{stronglyConnectedComponents, topologicalSort}
import io.github.alexandrepiveteau.graphs.builder.{addArc, addVertex, buildDirectedGraph}
import io.github.alexandrepiveteau.graphs.{->, Vertex}

import scala.collection.mutable

// TODO : Document this.
private def dependencies[T](rules: RulesDatabase[T], target: PredicateWithArity): Set[PredicateWithArity] =
  val visited = mutable.Set[PredicateWithArity]()
  val queue = mutable.ArrayDeque.empty[PredicateWithArity]
  queue += target
  while queue.nonEmpty do
    val predicate = queue.removeHead()
    if visited.add(predicate) then
      for rule <- rules(predicate) do {
        for clause <- rule.body do {
          queue.addOne(PredicateWithArity(clause.predicate, clause.arity))
        }
      }
  visited.toSet

// TODO : Document this.
private def stratify(predicates: Set[PredicateWithArity], database: RulesDatabase[_]): List[Set[PredicateWithArity]] =
  // 1. Compute the different strata using the rules.
  // 2. Use a topological sort to order the strata for evaluation.
  val rulesToVertices = mutable.Map[PredicateWithArity, Vertex]()
  val verticesToRules = mutable.Map[Vertex, PredicateWithArity]()
  val graph = buildDirectedGraph {
    predicates.foreach { id =>
      val vertex = addVertex()
      rulesToVertices(id) = vertex
      verticesToRules(vertex) = id
    }
    predicates.foreach { id =>
      database(id).foreach { rule =>
        rule.body.foreach { clause =>
          val fromKey = PredicateWithArity(clause.predicate, clause.arity)
          val toKey = PredicateWithArity(rule.head.predicate, rule.head.arity)
          val from = rulesToVertices(fromKey)
          val to = rulesToVertices(toKey)
          addArc(from -> to)
        }
      }
    }
  }

  val (scc, map) = graph.stronglyConnectedComponents()
  val strata = mutable.Map[Vertex, mutable.Set[PredicateWithArity]]()

  map.foreach((v, component) =>
    val stratum = strata.getOrElseUpdate(component, mutable.Set())
    stratum.add(verticesToRules(v))
  )

  scc.topologicalSort().toList.map(v => strata(v).toSet)

// TODO : Document this.
private def hasInvalidDependency[T](strata: List[Set[PredicateWithArity]], rules: RulesDatabase[T])
                                   (invalid: (Set[PredicateWithArity], Rule[T]) => Boolean): Boolean =
  val seen = mutable.Set[PredicateWithArity]()
  var hasInvalid = false
  for stratum <- strata do
    for predicate <- stratum do
      for rule <- rules(predicate) do
        if invalid(seen.toSet, rule) then hasInvalid = true
    seen.addAll(stratum)
  hasInvalid

// TODO : Document this.
private def hasCycle[T](strata: List[Set[PredicateWithArity]], rules: RulesDatabase[T]): Boolean =
  hasInvalidDependency(strata, rules) { (seen, rule) =>
    var invalid = false
    rule match
      case CombinationRule(_, body) =>
        // Cyclic dependencies are not allowed for negated clauses.
        for clause <- body do
          val other = PredicateWithArity(clause.predicate, clause.arity)
          if clause.negated && !seen.contains(other) then invalid = true
      case AggregationRule(head, clause, aggregate) =>
        // Cyclic dependencies are not allowed for aggregation rules.
        val predicate = PredicateWithArity(clause.predicate, clause.arity)
        if !seen.contains(predicate) then invalid = true
    invalid
  }

// TODO : Implement this.
// TODO : Document this.
def stratifiedEval[C: Context, O[_], R[_]](target: PredicateWithArity, idb: RulesDatabase[C],
                                           base: Database, result: Database)
                                          (evalStrata: (RulesDatabase[C], Database, Database) => O[Unit])
                                          (using op: IROp[C, O, R]): O[Unit] =
  val order = stratify(dependencies(idb, target), idb)
  if hasCycle(order, idb) then
    throw NoStratificationException()

  op.sequence(
    order.flatMap(stratum =>
      val rules = idb.filter(stratum)
      List(evalStrata(rules, base, result), op.mergeAndClear())
    )
  )
