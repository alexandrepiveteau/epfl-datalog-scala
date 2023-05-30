package io.github.alexandrepiveteau.graphs

trait Graph {
  val size: Int

  def contains(vertex: Vertex): Boolean = apply(vertex) >= 0 && apply(vertex) < size

  def apply(vertex: Vertex): Int

  def apply(index: Int): Vertex

  def neighborSize(index: Int): Int

  def neighborSize(vertex: Vertex): Int = neighborSize(apply(vertex))

  def neighbor(index: Int, neighborIndex: Int): Vertex

  def neighbor(vertex: Vertex, neighborIndex: Int): Vertex = neighbor(apply(vertex), neighborIndex)

  def apply(vertex: Int, neighborIndex: Int): Vertex = neighbor(vertex, neighborIndex)

  def apply(vertex: Vertex, neighborIndex: Int): Vertex = neighbor(vertex, neighborIndex)
}
