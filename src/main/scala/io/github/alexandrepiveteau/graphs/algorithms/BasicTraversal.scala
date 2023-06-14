package io.github.alexandrepiveteau.graphs.algorithms

import io.github.alexandrepiveteau.graphs.*

extension (graph: Graph)

  /**
   * Performs the given [[action]] on each vertex of this graph.
   *
   * ## Asymptotic complexity
   * - **Time complexity** : `O(|V|)`, where `|V|` is the number of vertices in the graph.
   * - **Space complexity** : `O(1)`.
   */
  def forEachVertex(action: Vertex => Unit): Unit =
    for i <- 0 until graph.size do
      action(graph(i))

  /**
   * Performs the given [[action]] on each neighbor of the given [[vertex]].
   *
   * ## Asymptotic complexity
   * - **Time complexity** : `O(|V|)`, where `|V|` is the number of neighbors of the given [[vertex]].
   * - **Space complexity** : `O(1)`.
   */
  def forEachNeighbor(vertex: Vertex)(action: Vertex => Unit): Unit =
    val index = graph(vertex)
    for (i <- 0 until graph.neighborSize(index)) do
      action(graph(vertex, i))

extension (graph: DirectedGraph)

  /**
   * Performs the given [[action]] on each arc in this graph.
   *
   * ## Asymptotic complexity
   * - **Time complexity** : `O(|A| + |V|)`, where `|A|` is the number of arcs in the graph and `|V|`
   * is the number of vertices in the graph.
   * - **Space complexity** : `O(1)`.
   */
  def forEachArc(action: Arc => Unit): Unit =
    graph.forEachVertex(v =>
      graph.forEachNeighbor(v)(w => action(v -> w))
    )
