package io.github.alexandrepiveteau.graphs.algorithms

import io.github.alexandrepiveteau.graphs.algorithms.forEachVertexDepthFirstPostOrder
import io.github.alexandrepiveteau.graphs.builder.{addArc, addVertex, buildDirectedGraph}
import io.github.alexandrepiveteau.graphs.{->, DirectedGraph, Vertex, transposed}

import scala.collection.mutable

extension (graph: DirectedGraph)

  // TODO : Document this.
  def stronglyConnectedComponents(): (DirectedGraph, Map[Vertex, Vertex]) =
    // 1. Traverse all vertices in post-order fashion, and prepend them to the queue.
    val visited = Array.fill(graph.size)(false)
    val queue = mutable.ArrayDeque.empty[Int]
    graph.forEachVertex { v =>
      if !visited(graph(v)) then
        graph.forEachVertexDepthFirstPostOrder(v, visited)(x => queue.prepend(graph(x)))
    }

    // 2. Take the transposed graph, and assign the vertices.
    val components = Array.fill(graph.size)(-1)
    val transposed = graph.transposed()
    val assigned = Array.fill(graph.size)(false)
    val order = queue.map(Vertex.apply).toArray
    var nextVertex = 0
    for (i <- order.indices)
      val v = order(i)
      if !assigned(graph(v)) then
        transposed.forEachVertexDepthFirst(v, assigned)(it => components(graph(it)) = nextVertex)
        nextVertex += 1

    // 3. Build the resulting graph, and the vertex map.
    val res = buildDirectedGraph {
      val vertices = Array.fill(nextVertex)(addVertex())
      graph.forEachArc((u, v) =>
        val cu = vertices(components(graph(u)))
        val cv = vertices(components(graph(v)))
        if cu != cv then addArc(cu -> cv)
      )
    }

    val map = components.zipWithIndex.map((v, i) => Vertex(i) -> graph(components(v))).toMap
    (res, map)
