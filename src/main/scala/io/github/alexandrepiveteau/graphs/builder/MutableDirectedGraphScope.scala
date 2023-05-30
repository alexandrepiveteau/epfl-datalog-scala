package io.github.alexandrepiveteau.graphs.builder

import io.github.alexandrepiveteau.graphs.Arc

trait MutableDirectedGraphScope extends MutableGraphScope {
  def addArc(arc: Arc): Unit
}

def addArc(arc: Arc)(using scope: MutableDirectedGraphScope): Unit =
  scope.addArc(arc)
