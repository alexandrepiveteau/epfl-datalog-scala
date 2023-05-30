package io.github.alexandrepiveteau.graphs.internal.graphs

import io.github.alexandrepiveteau.graphs.{DirectedGraph, Graph, NoSuchVertexException, Vertex}

class AdjacencyListGraph(private val neighbors: Array[Array[Vertex]]) extends Graph:

  override val size: Int = neighbors.length

  override def apply(vertex: Vertex): Int =
    if (vertex.index < 0 || vertex.index >= size) throw NoSuchVertexException()
    vertex.index

  override def apply(index: Int): Vertex =
    if (index < 0 || index >= size) throw IndexOutOfBoundsException()
    Vertex(index)

  override def neighborSize(index: Int): Int =
    if (index < 0 || index >= size) throw IndexOutOfBoundsException()
    neighbors(index).length

  override def neighbor(index: Int, neighborIndex: Int): Vertex =
    if (index < 0 || index >= size) throw IndexOutOfBoundsException()
    val n = neighbors(index)
    if (neighborIndex < 0 || neighborIndex >= n.length) throw IndexOutOfBoundsException()
    n(neighborIndex)

class AdjacencyListDirectedGraph(private val neighbors: Array[Array[Vertex]]) extends AdjacencyListGraph(neighbors), DirectedGraph:

  override def contains(arc: (Vertex, Vertex)): Boolean =
    val (u, v) = arc
    if (!contains(u) || !contains(v)) return false
    // TODO : Use binary search here.
    neighbors(u.index).contains(v)
