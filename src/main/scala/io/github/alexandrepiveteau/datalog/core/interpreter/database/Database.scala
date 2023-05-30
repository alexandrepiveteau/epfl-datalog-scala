package io.github.alexandrepiveteau.datalog.core.interpreter.database

import io.github.alexandrepiveteau.datalog.core.interpreter.algebra.{Relation, distinct, union}
import io.github.alexandrepiveteau.datalog.core.rule.{Fact, Rule}

import scala.annotation.targetName
import scala.collection.mutable

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
  @targetName("plus")
  def +(other: FactsDatabase[T]): FactsDatabase[T]

  // TODO : Document this.
  def toMutableFactsDatabase(): MutableFactsDatabase[T]

extension[T] (db: FactsDatabase[T])

  // TODO : Document this.
  def isEmpty: Boolean = db.iterator.forall(p => db(p).tuples.isEmpty)

  // TODO : Document this.
  def nonEmpty: Boolean = !db.isEmpty

// TODO : Document this.
trait MutableFactsDatabase[T] extends FactsDatabase[T]:

  // TODO : Document this.
  @targetName("plusAssign")
  def +=(other: FactsDatabase[T]): Unit

  // TODO : Document this.
  def update(predicate: PredicateWithArity, relation: Relation[T]): Unit

// TODO : Document this.
object MutableFactsDatabase:

  // TODO : Document this.
  def empty[T]: MutableFactsDatabase[T] =
    MapMutableFactsDatabase(mutable.Map.empty)

  // TODO : Document this.
  def builder[T](): FactsDatabaseBuilder[T] =
    new FactsDatabaseBuilder[T]:
      private val map = mutable.Map[PredicateWithArity, mutable.Set[Fact[T]]]()

      override def add(predicate: PredicateWithArity, fact: Fact[T]): Unit =
        if predicate.arity != fact.size then throw new IllegalArgumentException()
        map.getOrElseUpdate(predicate, mutable.Set.empty) += fact

      override def build(): FactsDatabase[T] =
        val values = map.view.map { case (k, v) => (k, Relation(k.arity, v.toSet)) }
        MapMutableFactsDatabase[T](mutable.Map.from(values))

// TODO : Document this.
def buildFactsDatabase[T](builder: FactsDatabaseBuilder[T] ?=> Unit): FactsDatabase[T] =
  val facts: FactsDatabaseBuilder[T] = MutableFactsDatabase.builder()
  builder(using facts)
  facts.build()

// TODO : Document this.
trait FactsDatabaseBuilder[T] extends FactsDatabaseBuilderScope[T]:

  // TODO : Document this.
  def build(): FactsDatabase[T]

private case class MapMutableFactsDatabase[T](map: mutable.Map[PredicateWithArity, Relation[T]]) extends MutableFactsDatabase[T]:

  @targetName("plusAssign")
  override def +=(other: FactsDatabase[T]): Unit =
    other.iterator.foreach { predicate =>
      val updated = this (predicate).union(other(predicate)).distinct()
      update(predicate, updated)
    }

  override def update(predicate: PredicateWithArity, relation: Relation[T]): Unit =
    map.update(predicate, relation)

  override def apply(predicate: PredicateWithArity): Relation[T] =
    map.getOrElse(predicate, Relation.empty(predicate.arity))

  override def iterator: Iterator[PredicateWithArity] =
    map.keys.iterator

  @targetName("plus")
  override def +(other: FactsDatabase[T]): FactsDatabase[T] =
    val result = toMutableFactsDatabase()
    result += other
    result

  override def toMutableFactsDatabase(): MutableFactsDatabase[T] =
    MapMutableFactsDatabase(map.clone())
