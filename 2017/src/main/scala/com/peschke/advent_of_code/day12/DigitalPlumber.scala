package com.peschke.advent_of_code
package day12

import cats.{Eq, Show}
import cats.instances.set._
import cats.instances.string._
import cats.instances.vector._

import scala.util.Try

/**
  * http://adventofcode.com/2017/day/12
  *
  * --- Day 12: Digital Plumber ---
  *
  * Walking along the memory banks of the stream, you find a small
  * village that is experiencing a little confusion: some programs
  * can't communicate with each other.
  *
  * Programs in this village communicate using a fixed system of
  * pipes. Messages are passed between programs using these pipes, but
  * most programs aren't connected to each other directly. Instead,
  * programs pass messages between each other until the message
  * reaches the intended recipient.
  *
  * For some reason, though, some of these messages aren't ever
  * reaching their intended recipient, and the programs suspect that
  * some pipes are missing. They would like you to investigate.
  *
  * You walk through the village and record the ID of each program and
  * the IDs with which it can communicate directly (your puzzle
  * input). Each program has one or more programs with which it can
  * communicate, and these pipes are bidirectional; if 8 says it can
  * communicate with 11, then 11 will say it can communicate with 8.
  *
  * You need to figure out how many programs are in the group that
  * contains program ID 0.
  *
  * For example, suppose you go door-to-door like a travelling
  * salesman and record the following list:
  *
  * {{{
  * 0 <-> 2
  * 1 <-> 1
  * 2 <-> 0, 3, 4
  * 3 <-> 2, 4
  * 4 <-> 2, 3, 6
  * 5 <-> 6
  * 6 <-> 4, 5
  * }}}
  *
  * In this example, the following programs are in the group that
  * contains program ID 0:
  *
  * - Program 0 by definition.
  * - Program 2, directly connected to program 0.
  * - Program 3 via program 2.
  * - Program 4 via program 2.
  * - Program 5 via programs 6, then 4, then 2.
  * - Program 6 via programs 4, then 2.
  *
  * Therefore, a total of 6 programs are in this group; all but
  * program 1, which has a pipe that connects it to itself.
  *
  * How many programs are in the group that contains program ID 0?
  *
  * --- Part Two ---
  *
  * There are more programs than just the ones in the
  * group containing program ID 0. The rest of them have no way of
  * reaching that group, and still might have no way of reaching each
  * other.
  *
  * A group is a collection of programs that can all communicate via
  * pipes either directly or indirectly. The programs you identified
  * just a moment ago are all part of the same group. Now, they would
  * like you to determine the total number of groups.
  *
  * In the example above, there were 2 groups: one consisting of
  * programs 0,2,3,4,5,6, and the other consisting solely of
  * program 1.
  *
  * How many groups are there in total?
  */
object DigitalPlumber extends AdventOfCodeDay {
  type P1 = Int
  type P2 = Int

  def runPart1(input: String): Try[Int] = Part1.numberOfConnections(Program("0"), input)
  def runPart2(input: String): Try[Int] = Part2.countGroups(input)

  private val p0 = Program("0")
  private val p1 = Program("1")
  private val p2 = Program("2")
  private val p3 = Program("3")
  private val p4 = Program("4")
  private val p5 = Program("5")
  private val p6 = Program("6")

  implicit val programEq: Eq[Program] = cats.derive.eq[Program]
  implicit val programShow: Show[Program] = cats.derive.show[Program]

  implicit val pipeEq: Eq[Pipe] = cats.derive.eq[Pipe]
  implicit val pipeShow: Show[Pipe] = cats.derive.show[Pipe]

  private val rawPipes = Vector(
    Pipe(p0, p2),
    Pipe(p1, p1),
    Pipe(p2, p3),
    Pipe(p2, p4),
    Pipe(p3, p4),
    Pipe(p4, p6),
    Pipe(p5, p6))

  def verifyPart1Samples(): Unit = {
    val input = """|0 <-> 2
                   |1 <-> 1
                   |2 <-> 0, 3, 4
                   |3 <-> 2, 4
                   |4 <-> 2, 3, 6
                   |5 <-> 6
                   |6 <-> 4, 5""".stripMargin

    println(verifyResult(Part1.parse)(input, rawPipes))

    val findConnectionsTest = (Part1.findConnections(_: Program, rawPipes)).liftedToTry
    Seq(
      p0 -> Set(p0, p2, p3, p4, p5, p6),
      p1 -> Set(p1),
      p2 -> Set(p0, p2, p3, p4, p5, p6),
      p3 -> Set(p0, p2, p3, p4, p5, p6),
      p4 -> Set(p0, p2, p3, p4, p5, p6),
      p5 -> Set(p0, p2, p3, p4, p5, p6),
      p6 -> Set(p0, p2, p3, p4, p5, p6)
    ).map((verifyResult(findConnectionsTest) _).tupled).foreach(println)
  }

  def verifyPart2Samples(): Unit = {
    println(verifyResult((Part2.separateGroups _).liftedToTry)(
      rawPipes,
      Set(
        Set(p0, p2, p3, p4, p5, p6),
        Set(p1))))
  }
}
