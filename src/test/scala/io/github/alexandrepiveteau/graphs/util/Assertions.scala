package io.github.alexandrepiveteau.graphs.util

import io.github.alexandrepiveteau.graphs.Graph
import org.scalatest.matchers.should.Matchers.shouldEqual as sE

extension (actual: Graph)

  /**
   * Asserts that the two [[Graph]]s are equal.
   */
  def shouldEqual(expected: Graph): Unit =
    actual.size sE expected.size
    for i <- 0 until actual.size do
      actual.neighborSize(i) sE expected.neighborSize(i)
      for j <- 0 until actual.neighborSize(i) do
        actual.neighbor(i, j) sE expected.neighbor(i, j)
