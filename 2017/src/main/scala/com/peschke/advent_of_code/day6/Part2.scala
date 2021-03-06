package com.peschke.advent_of_code
package day6

import scala.util.{Try, Failure, Success}

object Part2 {
  def cycleLength(input: String): Try[Int] = {
    def loop(prev: Memory.Region, seen: Vector[Memory.Region]): Try[Int] =
      prev.reallocate match {
        case None => Failure(
          new AdventOfCodeDayFailure(
            MemoryReallocation,
            input,
            new NoSuchElementException(s"Unable to reallocate $prev")))
        case Some(current) =>
          seen.indexOf(current) match {
            case -1 => loop(current, seen :+ current)
            case i => Success(seen.size - i)
          }
      }

    MemoryReallocation.parse(input).flatMap { region =>
      loop(region, Vector(region))
    }
  }.mapError(MemoryReallocation, input)
}
