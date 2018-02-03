package com.peschke.advent_of_code
package day24

import cats.Show
import cats.syntax.show._
import cats.instances.int._
import cats.instances.vector._

import scala.util.Try

/**
  * http://adventofcode.com/2017/day/24
  *
  * --- Day 24: Electromagnetic Moat ---
  *
  * The CPU itself is a large, black building surrounded by a
  * bottomless pit. Enormous metal tubes extend outward from the side
  * of the building at regular intervals and descend down into the
  * void. There's no way to cross, but you need to get inside.
  *
  * No way, of course, other than building a bridge out of the
  * magnetic components strewn about nearby.
  *
  * Each component has two ports, one on each end. The ports come in
  * all different types, and only matching types can be connected. You
  * take an inventory of the components by their port types (your
  * puzzle input). Each port is identified by the number of pins it
  * uses; more pins mean a stronger connection for your bridge. A 3/7
  * component, for example, has a type-3 port on one side, and a
  * type-7 port on the other.
  *
  * Your side of the pit is metallic; a perfect surface to connect a
  * magnetic, zero-pin port. Because of this, the first port you use
  * must be of type 0. It doesn't matter what type of port you end
  * with; your goal is just to make the bridge as strong as possible.
  *
  * The strength of a bridge is the sum of the port types in each
  * component. For example, if your bridge is made of components 0/3,
  * 3/7, and 7/4, your bridge has a strength of 0+3 + 3+7 + 7+4 = 24.
  *
  * For example, suppose you had the following components:
  *
  * {{{
  * 0/2
  * 2/2
  * 2/3
  * 3/4
  * 3/5
  * 0/1
  * 10/1
  * 9/10
  * }}}
  *
  * With them, you could make the following valid bridges:
  *
  * {{{
  * 0/1
  * 0/1--10/1
  * 0/1--10/1--9/10
  * 0/2
  * 0/2--2/3
  * 0/2--2/3--3/4
  * 0/2--2/3--3/5
  * 0/2--2/2
  * 0/2--2/2--2/3
  * 0/2--2/2--2/3--3/4
  * 0/2--2/2--2/3--3/5
  * }}}
  *
  * (Note how, as shown by 10/1, order of ports within a component
  * doesn't matter. However, you may only use each port on a component
  * once.)
  *
  * Of these bridges, the strongest one is 0/1--10/1--9/10; it has a
  * strength of 0+1 + 1+10 + 10+9 = 31.
  *
  * What is the strength of the strongest bridge you can make with the
  * components you have available?
  *
  * --- Part Two ---
  *
  * The bridge you've built isn't long enough; you can't jump the rest
  * of the way.
  *
  * In the example above, there are two longest bridges:
  *
  * {{{
  * 0/2--2/2--2/3--3/4
  * 0/2--2/2--2/3--3/5
  * }}}
  *
  * Of them, the one which uses the 3/5 component is stronger; its
  * strength is 0+2 + 2+2 + 2+3 + 3+5 = 19.
  *
  * What is the strength of the longest bridge you can make? If you
  * can make multiple bridges of the longest length, pick the
  * strongest one.
  */
object ElectromagneticMoat extends AdventOfCodeDay {
  override type P1 = Int
  override type P2 = Int

  override def runPart1(input: String): Try[P1] = Part1.howStrongIsTheStrongestBridge(input)

  override def runPart2(input: String): Try[P2] = Part2.howStrongIsTheLongestBridge(input)

  private val sampleInput =
    """0/2
      |2/2
      |2/3
      |3/4
      |3/5
      |0/1
      |10/1
      |9/10""".stripMargin

  override def verifyPart1Samples(): Unit = {
    implicit val vectorBridgeShow: Show[Vector[Bridge]] = unwrappedVectorShow[Bridge]
    implicit val vectorComponentShow: Show[Vector[Component]] = unwrappedVectorShow[Component]

    val components = Vector(
      Component(0, 2),
      Component(2, 2),
      Component(2, 3),
      Component(3, 4),
      Component(3, 5),
      Component(0, 1),
      Component(10, 1),
      Component(9, 10))

    val bridges = Vector(
      Bridge(Component(1, 0) :: Nil),
      Bridge(Component(10, 1) :: Component(1, 0) :: Nil),
      Bridge(Component(9, 10) :: Component(10, 1) :: Component(1, 0) :: Nil),
      Bridge(Component(2, 0) :: Nil),
      Bridge(Component(2, 2) :: Component(2, 0) :: Nil),
      Bridge(Component(3, 2) :: Component(2, 2) :: Component(2, 0) :: Nil),
      Bridge(Component(4, 3) :: Component(3, 2) :: Component(2, 2) :: Component(2, 0) :: Nil),
      Bridge(Component(5, 3) :: Component(3, 2) :: Component(2, 2) :: Component(2, 0) :: Nil),
      Bridge(Component(3, 2) :: Component(2, 0) :: Nil),
      Bridge(Component(4, 3) :: Component(3, 2) :: Component(2, 0) :: Nil),
      Bridge(Component(5, 3) :: Component(3, 2) :: Component(2, 0) :: Nil))

    println("--- parsing ---")
    println(Parser.parse(sampleInput).asResult(components).show)

    println("--- sorting ---")
    println(Try(components.sorted).asResult {
      Vector(
        Component(0, 1),
        Component(0, 2),
        Component(10, 1),
        Component(2, 2),
        Component(2, 3),
        Component(3, 4),
        Component(3, 5),
        Component(9, 10))
    }.show)
    Seq(
      Vector(
        Bridge(Component(1, 0) :: Nil),
        Bridge(Component(2, 0) :: Nil)),
      Vector(
        Bridge(Component(3, 2) :: Component(2, 2) :: Component(2, 0) :: Nil),
        Bridge(Component(3, 4) :: Component(3, 2) :: Component(2, 0) :: Nil),
        Bridge(Component(5, 3) :: Component(3, 2) :: Component(2, 0) :: Nil)
      ),
      Vector(
        Bridge(Component(1, 0) :: Nil),
        Bridge(Component(2, 0) :: Nil),
        Bridge(Component(3, 2) :: Component(2, 2) :: Component(2, 0) :: Nil),
        Bridge(Component(3, 4) :: Component(3, 2) :: Component(2, 0) :: Nil),
        Bridge(Component(5, 3) :: Component(3, 2) :: Component(2, 0) :: Nil))
    ).flatMap { sorted =>
      Seq(
        Try(sorted.sorted).asResult(sorted),
        Try(sorted.reverse.sorted).asResult(sorted)
      ).map(_.show)
    }.foreach(println)

    println("--- building bridges ---")
    println(Try(Part1.createBridges(components)).asResult(bridges).show)

    println("--- strength identification ---")
    println(Part1.howStrongIsTheStrongestBridge(sampleInput).asResult(31).show)
  }

  override def verifyPart2Samples(): Unit = {
    println(Part2.howStrongIsTheLongestBridge(sampleInput).asResult(19).show)
  }
}
