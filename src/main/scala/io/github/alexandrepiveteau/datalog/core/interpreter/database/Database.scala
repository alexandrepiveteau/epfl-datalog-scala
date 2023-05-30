package io.github.alexandrepiveteau.datalog.core.interpreter.database

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.Relation
import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Rule}

// TODO : Document this.
trait RulesDatabase[T] extends (PredicateWithArity => Set[Rule[T]]):

  // TODO : Document this.
  def iterator: Iterator[PredicateWithArity]

  // TODO : Document this.
  def apply(key: PredicateWithArity): Set[Rule[T]]

  // TODO : Document this.
  def filter(keys: Iterable[PredicateWithArity]): RulesDatabase[T]

  // TODO : Document this.
  def contains(key: PredicateWithArity): Boolean


// TODO : Document this.
case class MapRulesDatabase[T](map: Map[PredicateWithArity, Set[Rule[T]]]) extends RulesDatabase[T]:

  override def iterator: Iterator[PredicateWithArity] = map.keys.iterator

  override def apply(key: PredicateWithArity): Set[Rule[T]] = map.getOrElse(key, Set.empty)

  override def filter(keys: Iterable[PredicateWithArity]): RulesDatabase[T] =
    MapRulesDatabase(map.view.filterKeys(it => map.keySet.contains(it)).toMap)

  override def contains(key: PredicateWithArity): Boolean =
    map.contains(key)

// TODO : Document this.
trait FactsDatabaseBuilderScope[-T]:

  // TODO : Document this.
  def add(predicate: PredicateWithArity, fact: Fact[T]): Unit

// TODO : Document this.
def add[T](predicate: PredicateWithArity, fact: Fact[T])(using builder: FactsDatabaseBuilderScope[T]): Unit =
  builder.add(predicate, fact)

// TODO : Document this.
trait FactsDatabase[T]:

  // TODO : Document this.
  def iterator: Iterator[PredicateWithArity]

  // TODO : Document this.
  def apply(predicate: PredicateWithArity): Relation[T]

  // TODO : Document this.
  def +(other: FactsDatabase[T]): FactsDatabase[T]

  // TODO : Document this.
  def toMutableFactsDatabase(): MutableFactsDatabase[T]

extension[T] (db: FactsDatabase[T])
  def isEmpty: Boolean = db.iterator.forall(p => db(p).tuples.isEmpty)
  def nonEmpty: Boolean = !db.isEmpty

// TODO : Document this.
trait MutableFactsDatabase[T] extends FactsDatabase[T]:

  // TODO : Document this.
  def +=(other: FactsDatabase[T]): Unit

  // TODO : Document this.
  def set(predicate: PredicateWithArity, relation: Relation[T]): Unit