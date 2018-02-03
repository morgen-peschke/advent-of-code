package com.peschke.advent_of_code
package day16

import cats.{Eq, Show}

import scala.util.Try
import cats.syntax.either._
import cats.instances.string._
import cats.instances.either._
import cats.syntax.show._

/**
  * http://adventofcode.com/2017/day/16
  *
  * --- Day 16: Permutation Promenade ---
  *
  * You come upon a very unusual sight; a group of programs here
  * appear to be dancing.
  *
  * There are sixteen programs in total, named a through p. They start
  * by standing in a line: a stands in position 0, b stands in
  * position 1, and so on until p, which stands in position 15.
  *
  * The programs' dance consists of a sequence of dance moves:
  *
  * - Spin, written sX, makes X programs move from the end to the
  *   front, but maintain their order otherwise. (For example, s3 on
  *   abcde produces cdeab).
  *
  * - Exchange, written xA/B, makes the programs at positions A and B
  *   swap places.
  *
  * - Partner, written pA/B, makes the programs named A and B swap
  *   places.
  *
  * For example, with only five programs standing in a line (abcde),
  * they could do the following dance:
  *
  * - s1, a spin of size 1: eabcd.
  * - x3/4, swapping the last two programs: eabdc.
  * - pe/b, swapping programs e and b: baedc.
  *
  * After finishing their dance, the programs end up in order baedc.
  *
  * You watch the dance for a while and record their dance moves (your
  * puzzle input). In what order are the programs standing after their
  * dance?
  *
  * --- Part Two ---
  *
  * Now that you're starting to get a feel for the dance moves, you
  * turn your attention to the dance as a whole.
  *
  * Keeping the positions they ended up in from their previous dance,
  * the programs perform it again and again: including the first
  * dance, a total of one billion (1000000000) times.
  *
  * In the example above, their second dance would begin with the
  * order baedc, and use the same dance moves:
  *
  * s1, a spin of size 1: cbaed.
  * x3/4, swapping the last two programs: cbade.
  * pe/b, swapping programs e and b: ceadb.
  *
  * In what order are the programs standing after their billion dances?
  */
object PermutationPromenade extends AdventOfCodeDay {
  type P1 = Part1.ErrorMsgOr[String]
  type P2 = Part1.ErrorMsgOr[String]

  def runPart1(input: String): Try[P1] = Part1.dance(input).map(_.map(_.render))
  def runPart2(input: String): Try[P2] = Part2.dance(input).map(_.map(_.render))

  implicit val danceLineEq: Eq[DanceLine] = Eq.fromUniversalEquals[DanceLine]
  implicit val danceMoveEq: Eq[DanceMove] = Eq.fromUniversalEquals[DanceMove]

  implicit val danceLineShow: Show[DanceLine] = Show.show[DanceLine](_.render)
  implicit val danceMoveShow: Show[DanceMove] = Show.fromToString[DanceMove]

  def verifyPart1Samples(): Unit = {
    import Part1.DanceLineOps

    println("--- parsing ---")
    Seq(
      "s1" -> Spin(1).asRight,
      "s17" -> Spin(17).asRight,
      "x3/4" -> Exchange(3, 4).asRight,
      "x13/12" -> Exchange(13, 12).asRight,
      "pe/b" -> Partner(Program.Name('e').right.get, Program.Name('b').right.get).asRight
    ).map((verifyResult(Parser.danceMove.tryToParse) _).tupled).foreach(println)

    println("--- dance moves ---")
    val initial = DanceLine.ofLength(5)

    val finalResult =
      Seq(
        Spin(1) -> "[eabcd]",
        Exchange(3, 4) -> "[eabdc]",
        Partner(Program.Name('e').right.get, Program.Name('b').right.get) -> "[baedc]"
      ).foldLeft(initial) {
        case (l @ Left(_), _) => l
        case (Right(danceLine), (step, expected)) =>
          val result = danceLine.dance(step)
          println(Try(result.render).asResult(expected).show)
          result.asRight
      }
    println(finalResult.map(_.render).asRightResult("[baedc]").show)

    println(verifyResult(Part1.dance(_: String, initial))("s1,x3/4,pe/b", finalResult))
  }

  def verifyPart2Samples(): Unit = {
    println {
      Part2
        .dance("s1,x3/4,pe/b", DanceLine.ofLength(5)).map(_.map(_.render))
        .asResult("[abcde]".asRight)
        .show
    }
  }
}
