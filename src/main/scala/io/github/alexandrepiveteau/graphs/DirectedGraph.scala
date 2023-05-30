package io.github.alexandrepiveteau.graphs

trait DirectedGraph extends Graph:
  def contains(arc: Arc): Boolean

