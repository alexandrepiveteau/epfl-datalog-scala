package io.github.alexandrepiveteau.graphs

import io.github.alexandrepiveteau.graphs.algorithms.{forEachArc, forEachVertex}
import io.github.alexandrepiveteau.graphs.builder.{addArc, addVertex, buildDirectedGraph}

trait DirectedGraph extends Graph:
  def contains(arc: Arc): Boolean

extension (graph: DirectedGraph)

  // TODO: Document this.
  def transposed(): DirectedGraph =
    buildDirectedGraph {
      graph.forEachVertex(_ => addVertex())
      graph.forEachArc(a => addArc(a.reversed()))
    }