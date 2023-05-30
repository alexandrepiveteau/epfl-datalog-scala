package io.github.alexandrepiveteau.graphs

type Arc = (Vertex, Vertex)

extension (arc: Arc)

  def contains(vertex: Vertex): Boolean =
    val (from, to) = arc
    from == vertex || to == vertex

  def reversed(): Arc =
    val (from, to) = arc
    (to, from)

infix def ->(from: Vertex, to: Vertex): Arc = (from, to)
