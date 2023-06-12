package io.github.alexandrepiveteau.graphs.algorithms

import io.github.alexandrepiveteau.graphs.{DirectedGraph, Vertex}

import scala.collection.mutable

extension (graph: DirectedGraph)

  // TODO : Document this.
  def topologicalSort(): Array[Vertex] =
    // 0. Set up a queue of vertices to add to the topological sort, and a boolean array to keep track
    // of the vertices that have already been sorted.
    val queue = mutable.ArrayDeque[Int]()
    val sorted = Array.fill(graph.size)(false)
    val result = Array.newBuilder[Vertex]

    // 1. Compute the number of incoming edges for each vertex.
    val edges = Array.fill(graph.size)(0)
    graph.forEachArc { (_, to) => edges(graph(to)) += 1 }

    // 2. Add all the vertices with no incoming edges to the queue.
    for i <- edges.indices do
      if edges(i) == 0 then
        queue += i
        sorted(i) = true

    // 3. While there are some vertices to add to the topological sort, add them and remove their
    // outgoing edges. If a vertex has no more outgoing edges and has not been sorted yet, add it to
    // the queue.
    while queue.nonEmpty do
      val vertex = queue.removeHead()
      result += graph(vertex)

      graph.forEachNeighbor(graph(vertex)) { it =>
        val to = graph(it)
        edges(to) -= 1
        if edges(to) == 0 && !sorted(to) then
          queue += to
          sorted(to) = true
      }

    // 4. If there are still some vertices that have not been sorted, then there is a cycle in the
    // graph. Throw an exception.
    if (result.length != graph.size) throw IllegalArgumentException("The graph contains a cycle.")

    // 5. Return the result.
    result.result()
