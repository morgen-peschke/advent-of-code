package com.peschke.advent_of_code.day5

import scala.util.{Try, Failure}

import com.peschke.advent_of_code.AdventOfCodeDay._

object Part1 {
  def followJumps(instructions: Instructions): Stream[Instructions] = {
    def loop(prev: Instructions): Stream[Instructions] = {
      prev.jumpOffsets.lift(prev.index) match {
        case None => Stream.empty[Instructions]
        case Some(offset) =>
          val current = prev.copy(
            index = prev.index + offset,
            jumpOffsets = prev.jumpOffsets.updated(prev.index, offset + 1))
          current #:: loop(current)
      }
    }
    instructions #:: loop(instructions)
  }

  def stepsUntilExit(input: String): Try[Int] =
    TrampolineMaze.parse(input).map(followJumps).map(_.length - 1)
      .wrapFailure(throwable => Failure(new TrampolineMazeFailure(input, throwable)))
}
