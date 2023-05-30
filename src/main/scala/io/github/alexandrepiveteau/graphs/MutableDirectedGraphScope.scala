package io.github.alexandrepiveteau.graphs

trait MutableDirectedGraphScope extends MutableGraphScope {
  def addArc(arc: Arc): Unit
}

def addArc(arc: Arc)(using scope: MutableDirectedGraphScope): Unit =
  scope.addArc(arc)
