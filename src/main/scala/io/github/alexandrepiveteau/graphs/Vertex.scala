package io.github.alexandrepiveteau.graphs

case class Vertex(val index: Int)

object Vertex:
  val Invalid: Vertex = Vertex(-1)
