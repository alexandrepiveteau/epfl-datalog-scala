package io.github.alexandrepiveteau.datalog.core.interpreter.database

import io.github.alexandrepiveteau.datalog.core.interpreter.ir.Database

import scala.collection.mutable

// TODO : Document this.
class StorageManager[T] {

  // TODO : Document this.
  private val databases = mutable.Map[Database, MutableFactsDatabase[T]]()

  // TODO : Document this.
  def database(database: Database): MutableFactsDatabase[T] =
    if database == Database.Empty then MutableFactsDatabase.empty[T]
    else databases.getOrElseUpdate(database, MutableFactsDatabase.empty[T])

  // TODO : Document this.
  def removeAll(keep: Set[Database]): Unit =
    val toRemove = databases.keySet -- keep
    toRemove.foreach(databases.remove)
}
