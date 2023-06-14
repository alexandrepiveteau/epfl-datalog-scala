package io.github.alexandrepiveteau.graphs.algorithms

import io.github.alexandrepiveteau.graphs.->
import io.github.alexandrepiveteau.graphs.builder.{addArc, addVertex, buildDirectedGraph}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{empty, shouldBe, shouldEqual}

class TopologicalSortTestSuite extends AnyFunSuite:

  test("empty graph has empty topological sort") {
    val graph = buildDirectedGraph {}
    val order = graph.topologicalSort()
    order shouldBe empty
  }

  test("simple graph with two vertices") {
    val graph = buildDirectedGraph {
      val v0 = addVertex()
      val v1 = addVertex()
      addArc(v1 -> v0)
    }
    val order = graph.topologicalSort()
    val expected = Array(graph(1), graph(0))
    order shouldEqual expected
  }

  test("geeks for geeks example") {
    val graph = buildDirectedGraph {
      val v0 = addVertex()
      val v1 = addVertex()
      val v2 = addVertex()
      val v3 = addVertex()
      val v4 = addVertex()
      val v5 = addVertex()

      addArc(v5 -> v2)
      addArc(v5 -> v0)
      addArc(v4 -> v0)
      addArc(v4 -> v1)
      addArc(v2 -> v3)
      addArc(v3 -> v1)
    }
    val order = graph.topologicalSort()
    val expected = Array(graph(4), graph(5), graph(0), graph(2), graph(3), graph(1))
    order shouldEqual expected
  }
