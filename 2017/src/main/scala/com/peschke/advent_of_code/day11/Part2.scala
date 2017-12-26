package com.peschke.advent_of_code
package day11

import scala.util.Try

object Part2 {
  val origin = CubeCoord.origin[Int]

  def maxDistanceDuring(input: String): Try[Int] =
    Part1.parse(input)
      .map(_.foldLeft(origin -> 0) {
        case ((location, maxDistance), direction) =>
          val newLocation = location.move(direction)
          (newLocation, maxDistance.max(newLocation.distanceTo(origin)))
      })
      .map(_._2)
      .mapError(HexEd, input)
}
