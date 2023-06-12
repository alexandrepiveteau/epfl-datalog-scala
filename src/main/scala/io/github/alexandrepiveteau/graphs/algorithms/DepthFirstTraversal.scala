package io.github.alexandrepiveteau.graphs.algorithms

import io.github.alexandrepiveteau.graphs.{Graph, Vertex}

import scala.collection.mutable

extension (graph: Graph)

  // TODO : Document this.
  def forEachVertexDepthFirst(from: Vertex,
                              visited: Array[Boolean] = Array.fill(graph.size)(false))
                             (action: Vertex => Unit): Unit =
    forEachVertexDepthFirstHelper(from, visited)(action)(_ => ())

  // TODO : Document this.
  def forEachVertexDepthFirstPostOrder(from: Vertex,
                                       visited: Array[Boolean] = Array.fill(graph.size)(false))
                                      (action: Vertex => Unit): Unit =
    forEachVertexDepthFirstHelper(from, visited)(_ => ())(action)

  // TODO : Document this.
  private def forEachVertexDepthFirstHelper(from: Vertex,
                                            visited: Array[Boolean] = Array.fill(graph.size)(false))
                                           (inOrderAction: Vertex => Unit)
                                           (postOrderAction: Vertex => Unit): Unit =
    val counts = Array.fill(graph.size)(0)
    val path = mutable.ArrayDeque[Int]()
    path += graph(from)
    while path.nonEmpty do
      val next = path.last
      if !visited(next) then
        inOrderAction(graph(next))
        visited(next) = true
      var found = false
      var keepGoing = true
      while keepGoing && counts(next) < graph.neighborSize(next) do
        val neighbor = graph(next, counts(next))
        counts(next) = counts(next) + 1
        if !visited(graph(neighbor)) then
          found = true
          path += graph(neighbor)
          keepGoing = false // TODO : Check correctness of break.
      if !found then
        val finished = path.removeLast()
        postOrderAction(graph(finished))
