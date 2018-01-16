package com.peschke.advent_of_code
package day19

import scala.util.Try

/**
  * http://adventofcode.com/2017/day/19
  *
  * --- Day 19: A Series of Tubes ---
  *
  * Somehow, a network packet got lost and ended up here. It's trying
  * to follow a routing diagram (your puzzle input), but it's confused
  * about where to go.
  *
  * Its starting point is just off the top of the diagram. Lines
  * (drawn with |, -, and +) show the path it needs to take, starting
  * by going down onto the only line connected to the top of the
  * diagram. It needs to follow this path until it reaches the end
  * (located somewhere within the diagram) and stop there.
  *
  * Sometimes, the lines cross over each other; in these cases, it
  * needs to continue going the same direction, and only turn left or
  * right when there's no other option. In addition, someone has left
  * letters on the line; these also don't change its direction, but it
  * can use them to keep track of where it's been. For example:
  *
  * {{{
  *      |
  *      |  +--+
  *      A  |  C
  *  F---|----E|--+
  *      |  |  |  D
  *      +B-+  +--+
  * }}}
  * 
  * Given this diagram, the packet needs to take the following path:
  *
  * - Starting at the only line touching the top of the diagram, it
  *   must go down, pass through A, and continue onward to the first
  *   +.
  * - Travel right, up, and right, passing through B in the process.
  * - Continue down (collecting C), right, and up (collecting D).
  * - Finally, go all the way left through E and stopping at F.
  *
  * Following the path to the end, the letters it sees on its path are
  * ABCDEF.
  *
  * The little packet looks up at you, hoping you can help it find the
  * way. What letters will it see (in the order it would see them) if
  * it follows the path? (The routing diagram is very wide; make sure
  * you view it without line wrapping.)
  *
  * --- Part Two ---
  *
  * The packet is curious how many steps it needs to go.
  *
  * For example, using the same routing diagram from the example
  * above...
  *
  * {{{
  *      |
  *      |  +--+
  *      A  |  C
  *  F---|--|-E---+
  *      |  |  |  D
  *      +B-+  +--+
  * }}}
  * 
  * ...the packet would go:
  *
  * - 6 steps down (including the first line at the top of the
  *   diagram).
  * - 3 steps right.
  * - 4 steps up.
  * - 3 steps right.
  * - 4 steps down.
  * - 3 steps right.
  * - 2 steps up.
  * - 13 steps left (including the F it stops on).
  *
  * This would result in a total of 38 steps.
  *
  * How many steps does the packet need to go?
  */
object ASeriesOfTubes extends AdventOfCodeDay {
  type P1 = String
  type P2 = Int

  def runPart1(input: String): Try[P1] =
    Part1.parse(input).flatMap(Part1.navigate).mapError(ASeriesOfTubes, input)

  def runPart2(input: String): Try[P2] =
    Part2.stepCount(input).mapError(ASeriesOfTubes, input)

  private val sampleInput: String =
    """|     |
       |     |  +--+
       |     A  |  C
       | F---|----E|--+
       |     |  |  |  D
       |     +B-+  +--+
       |""".stripMargin

  private val sampleRoutingDiagram =
    RoutingDiagram(
      locations = Map(
        Coord(0, 5) -> ║,
        Coord(1, 5) -> ║,
        Coord(1, 8) -> ╬,
        Coord(1, 9) -> ═,
        Coord(1, 10) -> ═,
        Coord(1, 11) -> ╬,
        Coord(2, 5) -> C('A'),
        Coord(2, 8) -> ║,
        Coord(2, 11) -> C('C'),
        Coord(3, 1) -> C('F'),
        Coord(3, 2) -> ═,
        Coord(3, 3) -> ═,
        Coord(3, 4) -> ═,
        Coord(3, 5) -> ║,
        Coord(3, 6) -> ═,
        Coord(3, 7) -> ═,
        Coord(3, 8) -> ═,
        Coord(3, 9) -> ═,
        Coord(3, 10) -> C('E'),
        Coord(3, 11) -> ║,
        Coord(3, 12) -> ═,
        Coord(3, 13) -> ═,
        Coord(3, 14) -> ╬,
        Coord(4, 5) -> ║,
        Coord(4, 8) -> ║,
        Coord(4, 11) -> ║,
        Coord(4, 14) -> C('D'),
        Coord(5, 5) -> ╬,
        Coord(5, 6) -> C('B'),
        Coord(5, 7) -> ═,
        Coord(5, 8) -> ╬,
        Coord(5, 11) -> ╬,
        Coord(5, 12) -> ═,
        Coord(5, 13) -> ═,
        Coord(5, 14) -> ╬
      ),
      start = Coord(0, 5))

  def verifyPart1Samples(): Unit = {
    println(verifyResult(Part1.parse)(sampleInput, sampleRoutingDiagram))
    println(verifyResult(Part1.navigate)(sampleRoutingDiagram, "ABCDEF"))
  }

  def verifyPart2Samples(): Unit = {
    import Part2.RoutingDiagramFoldLeft

    def reimplementNavigate(rd: RoutingDiagram): Try[String] =
      Try(rd.foldLeft(Vector.empty[Char]) { (accum: Vector[Char], tile: Tile) =>
        tile match {
        case C(l) => accum :+ l
        case _ => accum
        }
      }).map(_.mkString)

    println(verifyResult(reimplementNavigate)(sampleRoutingDiagram, "ABCDEF"))
    println(verifyResult(Part2.stepCount)(sampleInput, 38))
  }
}
