package io.github.alexandrepiveteau.graphs.builder

import io.github.alexandrepiveteau.graphs.Vertex

trait MutableGraphScope {

  def addVertex(): Vertex

  def addVertices(): Vertices = Vertices(this)
}

def addVertex()(using scope: MutableGraphScope): Vertex = scope.addVertex()
def addVertices()(using scope: MutableGraphScope): Vertices = scope.addVertices()

case class Vertices(scope: MutableGraphScope) {
  def unapply(): Seq[Vertex] =
    LazyList.continually(scope.addVertex())
}
