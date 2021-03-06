package com.peschke.advent_of_code
package day13

import cats.{Eq, Show}
import cats.instances.boolean._
import cats.instances.vector._
import cats.instances.tuple._

import scala.util.Try

/**
  * http://adventofcode.com/2017/day/13
  *
  * --- Day 13: Packet Scanners ---
  *
  * You need to cross a vast firewall. The firewall consists of
  * several layers, each with a security scanner that moves back and
  * forth across the layer. To succeed, you must not be detected by a
  * scanner.
  *
  * By studying the firewall briefly, you are able to record (in your
  * puzzle input) the depth of each layer and the range of the
  * scanning area for the scanner within it, written as depth:
  * range. Each layer has a thickness of exactly 1. A layer at depth 0
  * begins immediately inside the firewall; a layer at depth 1 would
  * start immediately after that.
  *
  * For example, suppose you've recorded the following:
  *
  * {{{
  *  0: 3
  *  1: 2
  *  4: 4
  *  6: 4
  * }}}
  *
  * This means that there is a layer immediately inside the firewall
  * (with range 3), a second layer immediately after that (with range
  * 2), a third layer which begins at depth 4 (with range 4), and a
  * fourth layer which begins at depth 6 (also with range
  * 4). Visually, it might look like this:
  *
  * {{{
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  * }}}
  *
  * Within each layer, a security scanner moves back and forth within
  * its range. Each security scanner starts at the top and moves down
  * until it reaches the bottom, then moves up until it reaches the
  * top, and repeats. A security scanner takes one picosecond to move
  * one step. Drawing scanners as S, the first few picoseconds look
  * like this:
  *
  * {{{
  * Picosecond 0:
  *  0   1   2   3   4   5   6
  * [S] [S] ... ... [S] ... [S]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  * Picosecond 1:
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... [ ] ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  * Picosecond 2:
  *  0   1   2   3   4   5   6
  * [ ] [S] ... ... [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [S]     [S]
  *                 [ ]     [ ]
  *
  * Picosecond 3:
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... [ ] ... [ ]
  * [S] [S]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [S]     [S]
  * }}}
  *
  * Your plan is to hitch a ride on a packet about to move through the
  * firewall. The packet will travel along the top of each layer, and
  * it moves at one layer per picosecond. Each picosecond, the packet
  * moves one layer forward (its first move takes it into layer 0),
  * and then the scanners move one step. If there is a scanner at the
  * top of the layer as your packet enters it, you are caught. (If a
  * scanner moves into the top of its layer while you are there, you
  * are not caught: it doesn't have time to notice you before you
  * leave.) If you were to do this in the configuration above, marking
  * your current position with parentheses, your passage through the
  * firewall would look like this:
  *
  * {{{
  * Initial state:
  *  0   1   2   3   4   5   6
  * [S] [S] ... ... [S] ... [S]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  * Picosecond 0:
  *  0   1   2   3   4   5   6
  * (S) [S] ... ... [S] ... [S]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * ( ) [ ] ... ... [ ] ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 1:
  *  0   1   2   3   4   5   6
  * [ ] ( ) ... ... [ ] ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] (S) ... ... [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [S]     [S]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 2:
  *  0   1   2   3   4   5   6
  * [ ] [S] (.) ... [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [S]     [S]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [ ] (.) ... [ ] ... [ ]
  * [S] [S]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [S]     [S]
  *
  *
  * Picosecond 3:
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... (.) [ ] ... [ ]
  * [S] [S]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [S]     [S]
  *
  *  0   1   2   3   4   5   6
  * [S] [S] ... (.) [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [S]     [S]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 4:
  *  0   1   2   3   4   5   6
  * [S] [S] ... ... ( ) ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [S]     [S]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... ( ) ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 5:
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... [ ] (.) [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [S] ... ... [S] (.) [S]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 6:
  *  0   1   2   3   4   5   6
  * [ ] [S] ... ... [S] ... (S)
  * [ ] [ ]         [ ]     [ ]
  * [S]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... [ ] ... ( )
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  * }}}
  *
  * In this situation, you are caught in layers 0 and 6, because your
  * packet entered the layer when its scanner was at the top when you
  * entered it. You are not caught in layer 1, since the scanner moved
  * into the top of the layer once you were already there.
  *
  * The severity of getting caught on a layer is equal to its depth
  * multiplied by its range. (Ignore layers in which you do not get
  * caught.) The severity of the whole trip is the sum of these
  * values. In the example above, the trip severity is 0*3 + 6*4 = 24.
  *
  * Given the details of the firewall you've recorded, if you leave
  * immediately, what is the severity of your whole trip?
  *
  * --- Part Two ---
  *
  * Now, you need to pass through the firewall without being caught -
  * easier said than done.
  *
  * You can't control the speed of the packet, but you can delay it
  * any number of picoseconds. For each picosecond you delay the
  * packet before beginning your trip, all security scanners move one
  * step. You're not in the firewall during this time; you don't enter
  * layer 0 until you stop delaying the packet.
  *
  * In the example above, if you delay 10 picoseconds (picoseconds 0 -
  * 9), you won't get caught:
  *
  * {{{
  * State after delaying:
  *  0   1   2   3   4   5   6
  * [ ] [S] ... ... [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [S]     [S]
  *                 [ ]     [ ]
  *
  * Picosecond 10:
  *  0   1   2   3   4   5   6
  * ( ) [S] ... ... [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [S]     [S]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * ( ) [ ] ... ... [ ] ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 11:
  *  0   1   2   3   4   5   6
  * [ ] ( ) ... ... [ ] ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [S] (S) ... ... [S] ... [S]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 12:
  *  0   1   2   3   4   5   6
  * [S] [S] (.) ... [S] ... [S]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [ ] (.) ... [ ] ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 13:
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... (.) [ ] ... [ ]
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [S] ... (.) [ ] ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [S]     [S]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 14:
  *  0   1   2   3   4   5   6
  * [ ] [S] ... ... ( ) ... [ ]
  * [ ] [ ]         [ ]     [ ]
  * [S]             [S]     [S]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... ( ) ... [ ]
  * [S] [S]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [S]     [S]
  *
  *
  * Picosecond 15:
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... [ ] (.) [ ]
  * [S] [S]         [ ]     [ ]
  * [ ]             [ ]     [ ]
  *                 [S]     [S]
  *
  *  0   1   2   3   4   5   6
  * [S] [S] ... ... [ ] (.) [ ]
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [S]     [S]
  *                 [ ]     [ ]
  *
  *
  * Picosecond 16:
  *  0   1   2   3   4   5   6
  * [S] [S] ... ... [ ] ... ( )
  * [ ] [ ]         [ ]     [ ]
  * [ ]             [S]     [S]
  *                 [ ]     [ ]
  *
  *  0   1   2   3   4   5   6
  * [ ] [ ] ... ... [ ] ... ( )
  * [S] [S]         [S]     [S]
  * [ ]             [ ]     [ ]
  *                 [ ]     [ ]
  * }}}
  * 
  * Because all smaller delays would get you caught, the fewest number
  * of picoseconds you would need to delay to get through safely is
  * 10.
  *
  * What is the fewest number of picoseconds that you need to delay
  * the packet to pass through the firewall without being caught?
  */
object PacketScanners extends AdventOfCodeDay {
  type P1 = Severity
  type P2 = Delay

  def runPart1(input: String): Try[Severity] = Part1.traverseFirewall(input)
  def runPart2(input: String): Try[Delay] = Part2.traverseFirewallSafely(input)

  private val input =
    """|0: 3
       |1: 2
       |4: 4
       |6: 4""".stripMargin

  implicit val directionEq: Eq[Direction] = Eq.fromUniversalEquals[Direction]
  implicit val directionShow: Show[Direction] = Show.fromToString[Direction]

  implicit val scannerEq: Eq[Scanner] = Eq.fromUniversalEquals[Scanner]
  implicit val scannerShow: Show[Scanner] = cats.derive.show[Scanner]

  implicit val delayEq: Eq[Delay] = Eq.fromUniversalEquals[Delay]
  implicit val delayShow: Show[Delay] = cats.derive.show[Delay]

  implicit val firewallEq: Eq[Firewall] = Eq.fromUniversalEquals[Firewall]
  implicit val firewallShow: Show[Firewall] = cats.derive.show[Firewall]

  implicit val severityEq: Eq[Severity] = Eq.fromUniversalEquals[Severity]
  implicit val severityShow: Show[Severity] = cats.derive.show[Severity]

  implicit val caughtEq: Eq[Caught] = Eq.fromUniversalEquals[Caught]
  implicit val caughtShow: Show[Caught] = cats.derive.show[Caught]

  val initial =
    Firewall(
      Layer.scanning(Scanner(3, 0, Down)),
      Layer.scanning(Scanner(2, 0, Down)),
      Layer.empty,
      Layer.empty,
      Layer.scanning(Scanner(4, 0, Down)),
      Layer.empty,
      Layer.scanning(Scanner(4, 0, Down)))

  def verifyPart1Samples(): Unit = {
    import Part1.FirewallOps

    println("--- Parsing ---")
    println(verifyResult(Part1.parse)(input, initial))

    println("--- Tick ---")
    val testTick = ((_: Firewall).tick).liftedToTry
    val time0 =
      Firewall(
        Layer.scanning(Scanner(3, 1, Down)).copy(packetInLayer = true),
        Layer.scanning(Scanner(2, 1, Down)),
        Layer.empty,
        Layer.empty,
        Layer.scanning(Scanner(4, 1, Down)),
        Layer.empty,
        Layer.scanning(Scanner(4, 1, Down)))
    val time1 =
      Firewall(
        Layer.scanning(Scanner(3, 2, Down)),
        Layer.scanning(Scanner(2, 0, Up)).copy(packetInLayer = true),
        Layer.empty,
        Layer.empty,
        Layer.scanning(Scanner(4, 2, Down)),
        Layer.empty,
        Layer.scanning(Scanner(4, 2, Down)))
    val time2 =
      Firewall(
        Layer.scanning(Scanner(3, 1, Up)),
        Layer.scanning(Scanner(2, 1, Down)),
        Layer.empty.copy(packetInLayer = true),
        Layer.empty,
        Layer.scanning(Scanner(4, 3, Down)),
        Layer.empty,
        Layer.scanning(Scanner(4, 3, Down)))
    val time3 =
      Firewall(
        Layer.scanning(Scanner(3, 0, Up)),
        Layer.scanning(Scanner(2, 0, Up)),
        Layer.empty,
        Layer.empty.copy(packetInLayer = true),
        Layer.scanning(Scanner(4, 2, Up)),
        Layer.empty,
        Layer.scanning(Scanner(4, 2, Up)))
    val time4 =
      Firewall(
        Layer.scanning(Scanner(3, 1, Down)),
        Layer.scanning(Scanner(2, 1, Down)),
        Layer.empty,
        Layer.empty,
        Layer.scanning(Scanner(4, 1, Up)).copy(packetInLayer = true),
        Layer.empty,
        Layer.scanning(Scanner(4, 1, Up)))
    val time5 =
      Firewall(
        Layer.scanning(Scanner(3, 2, Down)),
        Layer.scanning(Scanner(2, 0, Up)),
        Layer.empty,
        Layer.empty,
        Layer.scanning(Scanner(4, 0, Up)),
        Layer.empty.copy(packetInLayer = true),
        Layer.scanning(Scanner(4, 0, Up)))
    val time6 =
      Firewall(
        Layer.scanning(Scanner(3, 1, Up)),
        Layer.scanning(Scanner(2, 1, Down)),
        Layer.empty,
        Layer.empty,
        Layer.scanning(Scanner(4, 1, Down)),
        Layer.empty,
        Layer.scanning(Scanner(4, 1, Down)).copy(packetInLayer = true))
    Seq[(Firewall, (Firewall, Vector[Caught]))](
      (initial, (time0, Vector(Caught(0, Scanner(3, 0, Down))))),
      (time0, (time1, Vector.empty[Caught])),
      (time1, (time2, Vector.empty[Caught])),
      (time2, (time3, Vector.empty[Caught])),
      (time3, (time4, Vector.empty[Caught])),
      (time4, (time5, Vector.empty[Caught])),
      (time5, (time6, Vector(Caught(6, Scanner(4, 0, Up)))))
    ).map((verifyResult(testTick) _).tupled).foreach(println)

    println("--- Traversal ---")
    println(verifyResult(Part1.traverseFirewall)(input, Severity(24)))
  }

  def verifyPart2Samples(): Unit = {
    import Part2.FirewallOps

    val checkIfWouldBeCaught = (Part2.wouldBeCaught _).liftedToTry
    Iterator.iterate(initial)(_.delay).take(9)
      .map(verifyResult(checkIfWouldBeCaught)(_, true)).foreach(println)

    val time11 = Iterator.iterate(initial)(_.delay).drop(10).next
    println(verifyResult(checkIfWouldBeCaught)(time11, false))

    println(verifyResult(Part2.traverseFirewallSafely)(input, Delay(10)))
  }
}
