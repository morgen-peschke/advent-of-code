package com.peschke.advent_of_code
package day16

import scala.util.Try

import eu.timepit.refined.refineMV

object Part2 {
  import Part1.{ErrorMsgOr, DanceLineOps, parse}

  val OneBillion: Int = 1e9.toInt

  def dance(
    input: String,
    initialOrError: ErrorMsgOr[DanceLine] = DanceLine.ofLength(refineMV(16))
  ): Try[ErrorMsgOr[DanceLine]] =
    parse(input).map { routine =>
      @scala.annotation.tailrec
      def loop(
        current: DanceLine,
        iteration: Int = 0,
        memo: Map[DanceLine, DanceLine] = Map.empty
      ): DanceLine =
        if (iteration == OneBillion) current
        else memo.get(current) match {
          case None =>
            val result = current.dance(routine)
            loop(result, iteration + 1, memo + (current -> result))
          case Some(result) =>
            val newIteration = OneBillion - (OneBillion % iteration)
            if (newIteration == OneBillion) current
            else loop(result, newIteration + 1, memo)
        }

      initialOrError.map(loop(_))
    }
}
