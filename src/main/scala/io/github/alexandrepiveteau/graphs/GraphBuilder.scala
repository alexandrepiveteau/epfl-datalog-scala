package io.github.alexandrepiveteau.graphs

import scala.collection.mutable

// TODO : Document this.
trait GraphBuilder extends MutableGraphScope:

  // TODO : Document this.
  def toGraph(): Graph

// TODO : Document this.
abstract class MutableListGraphBuilder extends GraphBuilder:

  // TODO : Document this.
  protected val neighbors = mutable.ListBuffer[mutable.ListBuffer[Int]]()

  override def addVertex(): Vertex =
    neighbors += mutable.ListBuffer[Int]()
    Vertex(neighbors.size)

  // TODO : Document this.
  def checkLink(u: Vertex, v: Vertex): Unit =
    if (u.index < 0 || u.index >= neighbors.size) throw IndexOutOfBoundsException()
    if (v.index < 0 || v.index >= neighbors.size) throw IndexOutOfBoundsException()

// TODO : Document this.
def compactToVertexArray(neighbors: mutable.ListBuffer[mutable.ListBuffer[Int]]): Array[Array[Vertex]] =
  Array.tabulate(neighbors.size) { it =>
    var last = -1 // This is an invalid vertex.
    var count = 0
    val list = mutable.ListBuffer.from(neighbors(it).toList.sorted)
    for j <- list.indices do {
      if (list(j) != last) {
        list.update(count, list(j)) // Update the array in place.
        count += 1
        last = list(j)
      }
    }
    list.take(count).map(Vertex(_)).toArray
  }
