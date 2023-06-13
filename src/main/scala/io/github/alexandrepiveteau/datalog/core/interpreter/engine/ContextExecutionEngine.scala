package io.github.alexandrepiveteau.datalog.core.interpreter.engine

import io.github.alexandrepiveteau.datalog.core.Domain
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{FactsDatabase, PredicateWithArity, RulesDatabase, StorageManager}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Database
import io.github.alexandrepiveteau.datalog.core.interpreter.{Algorithm, Context, partition}
import io.github.alexandrepiveteau.datalog.core.rule.*

import scala.collection.mutable

// TODO : Document this.
private def constants[T](terms: List[Term[T]]): Set[Value[T]] =
  val values = mutable.Set.empty[Value[T]]
  for term <- terms do
    term match
      case value: Value[T] => values += value
      case _: Variable => ()
  values.toSet

// TODO : Document this.
private def constants[T](rule: Rule[T]): Set[Value[T]] =
  val values = mutable.Set.empty[Value[T]]
  values ++= constants(rule.head.terms)
  for clause <- rule.body do
    values ++= constants(clause.terms)
  values.toSet

// TODO : Document this.
private def constants[T](rules: RulesDatabase[T], facts: FactsDatabase[T]): Set[Value[T]] =
  val values = mutable.Set.empty[Value[T]]
  rules.iterator.foreach(predicate =>
    rules(predicate).foreach(rule =>
      values ++= constants(rule)
    )
  )
  facts.iterator.foreach(predicate =>
    facts(predicate).foreach(fact =>
      values ++= constants(fact)
    )
  )
  values.toSet

/**
 * An implementation of [[ExecutionEngine]] which uses a [[Context]] to store the constants of the
 * Datalog program, and delegates the evaluation to the [[evaluate]] method.
 *
 * @tparam T the type of the constants in the Datalog program.
 */
abstract class ContextExecutionEngine[T] extends ExecutionEngine[T]:

  private def ctx(idb: RulesDatabase[T], edb: FactsDatabase[T], domain: Domain[T]): Context[T] =
    Context(terms = constants(idb, edb), domain = domain)

  override def solve(predicate: Predicate, arity: Int, algorithm: Algorithm)
                    (using domain: Domain[T], rules: Set[Rule[T]]): Iterable[Fact[T]] =
    val (idb, edb) = partition(rules)
    val target = PredicateWithArity(predicate, arity)

    val storage = StorageManager[T]()
    storage.database(Database.Base) += edb

    evaluate(storage, target, idb, algorithm, domain, ctx(idb, edb, domain))

    val result = storage.database(Database.Base)
    result(target).tuples

  /**
   * Evaluates the [[target]] predicate, using the [[idb]] and [[edb]] databases, and the [[algorithm]].
   * The results should be stored in the [[storage]], in the [[Database.Base]] database, once the evaluation
   * is done.
   *
   * @param storage   the [[StorageManager]] used to store the results.
   * @param target    the [[PredicateWithArity]] to evaluate.
   * @param idb       the [[RulesDatabase]] containing the rules.
   * @param algorithm the [[Algorithm]] to use.
   * @param domain    the [[Domain]] of the Datalog program.
   * @param context   the [[Context]] of the Datalog program.
   */
  def evaluate(storage: StorageManager[T],
               target: PredicateWithArity,
               idb: RulesDatabase[T],
               algorithm: Algorithm,
               domain: Domain[T],
               context: Context[T]): Unit
  