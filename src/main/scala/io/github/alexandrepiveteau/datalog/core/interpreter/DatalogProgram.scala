package io.github.alexandrepiveteau.datalog.core.interpreter

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.{TupleSet, given}
import io.github.alexandrepiveteau.datalog.core.interpreter.database.{FactsDatabase, PredicateWithArity, RulesDatabase, StorageManager}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Database.{Base, Result}
import io.github.alexandrepiveteau.datalog.core.interpreter.ir.{*, given}
import io.github.alexandrepiveteau.datalog.core.rule.*
import io.github.alexandrepiveteau.datalog.core.{Domain, Program}

import scala.collection.mutable

// TODO : Document this.
private def constants[T](atoms: List[Atom[T]]): Set[Value[T]] =
  val values = mutable.Set.empty[Value[T]]
  for atom <- atoms do
    atom match
      case value: Value[T] => values += value
      case _: Variable => ()
  values.toSet

// TODO : Document this.
private def constants[T](rule: Rule[T]): Set[Value[T]] =
  val values = mutable.Set.empty[Value[T]]
  values ++= constants(rule.head.atoms)
  for clause <- rule.body do
    values ++= constants(clause.atoms)
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

// TODO : Document this.
class DatalogProgram[T](val domain: Domain[T],
                        val rules: Set[Rule[T]],
                        val algorithm: Algorithm) extends Program[T]:

  // TODO : Document this.
  private def ctx(idb: RulesDatabase[T], edb: FactsDatabase[T]): Context[T] =
    Context(atoms = constants(idb, edb), domain = domain)

  override def solve(predicate: Predicate, arity: Int): Iterable[Fact[T]] =
    val (idb, edb) = partition(rules)
    val target = PredicateWithArity(predicate, arity)

    given Context[T] = ctx(idb, edb)

    val storage = StorageManager[T]()
    storage.database(Base) += edb

    val eval = stratifiedEval[IROpInterpreter, TupleSet, T](target, idb, Base, Result)((i, e, r) => algorithm.evaluate(i, e, r))
    eval(storage)

    val result = storage.database(Base)
    result(target).tuples
