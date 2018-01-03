package com.peschke.advent_of_code
package day16

import scala.util.Try

import cats.syntax.either._

import eu.timepit.refined.refineMV

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
  */
object PermutationPromenade extends AdventOfCodeDay {
  type P1 = Part1.ErrorMsgOr[String]
  type P2 = Nothing

  def runPart1(input: String): Try[P1] = Part1.dance(input).map(_.map(_.render))
  def runPart2(input: String): Try[P2] = ???

  def verifyPart1Samples(): Unit = {
    import Part1.DanceMoveOps

    println("--- parsing ---")
    Seq(
      "s1" -> Spin(1).asRight,
      "s17" -> Spin(17).asRight,
      "x3/4" -> Exchange(refineMV(3), refineMV(4)).asRight,
      "x13/12" -> Exchange(refineMV(13), refineMV(12)).asRight,
      "pe/b" -> Partner(refineMV('e'), refineMV('b')).asRight
    ).map((verifyResult(Parser.danceMove.tryToParse) _).tupled).foreach(println)

    println("--- dance moves ---")
    val initial = DanceLine.ofLength(refineMV(5))

    val finalResult =
      Seq(
        Spin(1) -> "[eabcd]",
        Exchange(refineMV(3), refineMV(4)) -> "[eabdc]",
        Partner(refineMV('e'), refineMV('b')) -> "[baedc]"
      ).foldLeft(initial) {
        case (l @ Left(_), _) => l
        case (Right(danceLine), (step, expected)) =>
          val result = step.applyTo(danceLine)
          println(result.map(_.render).asResult(expected).render)
          result
      }
    println(finalResult.map(_.render).asResult("[baedc]").render)

    println(verifyResult(Part1.dance(_: String, initial))("s1,x3/4,pe/b", finalResult))
  }

  def verifyPart2Samples(): Unit = {}
}
