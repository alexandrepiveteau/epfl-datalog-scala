package io.github.alexandrepiveteau.graphs

import scala.annotation.targetName

type Arc = (Vertex, Vertex)

extension (arc: Arc)

  def contains(vertex: Vertex): Boolean =
    val (from, to) = arc
    from == vertex || to == vertex

  def reversed(): Arc =
    val (from, to) = arc
    (to, from)

@targetName("arcTo")
infix def ->(from: Vertex, to: Vertex): Arc = (from, to)
