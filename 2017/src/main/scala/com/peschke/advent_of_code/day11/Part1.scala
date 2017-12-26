package com.peschke.advent_of_code
package day11

import scala.util.Try

object Part1 {
  def parse(input: String): Try[Seq[Direction]] = Try {
    input.split(',').map(Direction.parse _).toSeq
  }

  val origin = CubeCoord.origin[Int]

  def distanceAfter(input: String): Try[Int] =
    parse(input)
      .map(_.foldLeft(origin)(_ move _) distanceTo origin)
      .mapError(HexEd, input)
}
