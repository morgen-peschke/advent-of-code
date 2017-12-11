package com.peschke.advent_of_code.day6

import scala.util.{Try, Failure, Success}

import com.peschke.advent_of_code.AdventOfCodeDay._

object Part1 {
  def stepsUntilCycle(input: String): Try[Int] = {
    def loop(prev: Memory.Region, seen: Set[Memory.Region]): Try[Int] =
      prev.reallocate match {
        case None => Failure(
          new MemoryReallocationFailure(input,
            new NoSuchElementException(s"Unable to reallocate $prev")))
        case Some(current) if seen.contains(current) => Success(seen.size)
        case Some(current) => loop(current, seen + current)
      }

    MemoryReallocation.parse(input).flatMap { region =>
      loop(region, Set(region))
    }
  }.mapError(new MemoryReallocationFailure(input, _))
}
