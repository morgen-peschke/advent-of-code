package com.peschke.advent_of_code
package day24

import scala.util.Try

object Part2 {
  import Part1.BridgeOps

  def howStrongIsTheLongestBridge(input: String): Try[Int] =
    Parser.parse(input)
      .map(Part1.createBridges)
      .map { bridges =>
        val maxLength = bridges.map(_.parts.length).max
        bridges
          .filter(_.parts.lengthCompare(maxLength) == 0)
          .map(_.strength)
          .max
      }
      .mapError(ElectromagneticMoat, input)
}
