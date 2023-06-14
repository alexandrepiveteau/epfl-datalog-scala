package io.github.alexandrepiveteau.graphs.algorithms

import io.github.alexandrepiveteau.graphs.->
import io.github.alexandrepiveteau.graphs.builder.{addArc, addVertex, buildDirectedGraph}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{empty, shouldBe, shouldEqual}

class StronglyConnectedComponentsTestSuite extends AnyFunSuite:

  // https://www.geeksforgeeks.org/strongly-connected-components/
  test("geeks for geeks example") {
    val graph = buildDirectedGraph {
      val v0 = addVertex()
      val v1 = addVertex()
      val v2 = addVertex()
      val v3 = addVertex()
      val v4 = addVertex()

      addArc(v0 -> v2)
      addArc(v2 -> v1)
      addArc(v1 -> v0)
      addArc(v0 -> v3)
      addArc(v3 -> v4)
    }
    val (scc, map) = graph.stronglyConnectedComponents()

    3 shouldEqual scc.size
    5 shouldEqual map.size

    // First SCC (v0, v1, v2).
    scc(map(graph(0))) shouldEqual scc(map(graph(1)))
    scc(map(graph(1))) shouldEqual scc(map(graph(2)))
    scc(map(graph(0))) shouldEqual scc(map(graph(2)))

    // Second SCC (v3).
    scc(map(graph(0))) shouldEqual scc(map(graph(3)))
    scc(map(graph(1))) shouldEqual scc(map(graph(3)))
    scc(map(graph(2))) shouldEqual scc(map(graph(3)))
    scc(map(graph(4))) shouldEqual scc(map(graph(3)))

    // Third SCC (v4).
    scc(map(graph(0))) shouldEqual scc(map(graph(4)))
    scc(map(graph(1))) shouldEqual scc(map(graph(4)))
    scc(map(graph(2))) shouldEqual scc(map(graph(4)))
    scc(map(graph(3))) shouldEqual scc(map(graph(4)))
  }

