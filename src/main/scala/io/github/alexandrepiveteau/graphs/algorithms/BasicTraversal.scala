package io.github.alexandrepiveteau.graphs.algorithms

import io.github.alexandrepiveteau.graphs.{Arc, ->, DirectedGraph, Graph, Vertex}

// TODO : Document this.
extension (graph: Graph)

  // TODO : Document this.
  def forEachVertex(action: Vertex => Unit): Unit =
    for i <- 0 until graph.size do
      action(graph(i))

  // TODO : Document this.
  def forEachNeighbor(vertex: Vertex)(action: Vertex => Unit): Unit =
    val index = graph(vertex)
    for (i <- 0 until graph.neighborSize(index)) do
      action(graph(vertex, i))

extension (graph: DirectedGraph)

  // TODO : Document this.
  def forEachArc(action: Arc => Unit): Unit =
    graph.forEachVertex(v =>
      graph.forEachNeighbor(v)(w => action(v -> w))
    )
