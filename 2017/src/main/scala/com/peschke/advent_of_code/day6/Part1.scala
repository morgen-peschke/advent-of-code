package com.peschke.advent_of_code
package day6

import scala.util.{Try, Failure, Success}

object Part1 {
  def stepsUntilCycle(input: String): Try[Int] = {
    def loop(prev: Memory.Region, seen: Set[Memory.Region]): Try[Int] =
      prev.reallocate match {
        case None => Failure(
          new AdventOfCodeDayFailure(
            MemoryReallocation,
            input,
            new NoSuchElementException(s"Unable to reallocate $prev")))
        case Some(current) if seen.contains(current) => Success(seen.size)
        case Some(current) => loop(current, seen + current)
      }

    MemoryReallocation.parse(input).flatMap { region =>
      loop(region, Set(region))
    }
  }.mapError(MemoryReallocation, input)
}
