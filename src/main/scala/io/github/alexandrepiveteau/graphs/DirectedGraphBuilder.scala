package io.github.alexandrepiveteau.graphs

import io.github.alexandrepiveteau.graphs.internal.graphs.AdjacencyListDirectedGraph

trait DirectedGraphBuilder extends GraphBuilder, MutableDirectedGraphScope:
  override def toGraph(): DirectedGraph

object DirectedGraphBuilder:
  def builder(): DirectedGraphBuilder = MutableListDirectedGraphBuilder()

def buildDirectedGraph(scope: MutableDirectedGraphScope ?=> Unit): DirectedGraph =
  val builder = DirectedGraphBuilder.builder()
  scope(using builder)
  builder.toGraph()

private class MutableListDirectedGraphBuilder extends MutableListGraphBuilder(), DirectedGraphBuilder:

  override def addArc(arc: (Vertex, Vertex)): Unit =
    val (u, v) = arc
    checkLink(u, v)
    neighbors(u.index) += v.index

  override def toGraph(): DirectedGraph = AdjacencyListDirectedGraph(compactToVertexArray(neighbors))
