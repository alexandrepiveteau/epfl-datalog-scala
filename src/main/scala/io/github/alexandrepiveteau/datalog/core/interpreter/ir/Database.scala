package io.github.alexandrepiveteau.datalog.core.interpreter.ir

// TODO : Document this.
case class Database(backing: String)

// TODO : Document this.
object Database {

  // TODO : Document this.
  val Empty: Database = Database("Empty")

  // TODO : Document this.
  val Base: Database = Database("Base")

  // TODO : Document this.
  val Result: Database = Database("Result")
}
