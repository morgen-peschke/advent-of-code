package com.peschke.advent_of_code
package day21

import scala.util.Try
import cats.syntax.show._
import cats.instances.int._
import cats.instances.string._

/**
  * http://adventofcode.com/2017/day/21
  *
  * --- Day 21: Fractal Art ---
  *
  * You find a program trying to generate some art. It uses a strange
  * process that involves repeatedly enhancing the detail of an image
  * through a set of rules.
  *
  * The image consists of a two-dimensional square grid of pixels that
  * are either on (#) or off (.). The program always begins with this
  * pattern:
  *
  * {{{
  * .#.
  * ..#
  * ###
  * }}}
  *
  * Because the pattern is both 3 pixels wide and 3 pixels tall, it is
  * said to have a size of 3.
  *
  * Then, the program repeats the following process:
  *
  * - If the size is evenly divisible by 2, break the pixels up into
  *   2x2 squares, and convert each 2x2 square into a 3x3 square by
  *   following the corresponding enhancement rule.
  * - Otherwise, the size is evenly divisible by 3; break the pixels
  *   up into 3x3 squares, and convert each 3x3 square into a 4x4
  *   square by following the corresponding enhancement rule.
  *
  * Because each square of pixels is replaced by a larger one, the
  * image gains pixels and so its size increases.
  *
  * The artist's book of enhancement rules is nearby (your puzzle
  * input); however, it seems to be missing rules. The artist explains
  * that sometimes, one must rotate or flip the input pattern to find
  * a match. (Never rotate or flip the output pattern, though.) Each
  * pattern is written concisely: rows are listed as single units,
  * ordered top-down, and separated by slashes. For example, the
  * following rules correspond to the adjacent patterns:
  *
  * {{{
  * ../.#  =  ..
  *           .#
  *
  *                 .#.
  * .#./..#/###  =  ..#
  *                 ###
  *
  *                         #..#
  * #..#/..../#..#/.##.  =  ....
  *                         #..#
  *                         .##.
  * }}}
  *
  * When searching for a rule to use, rotate and flip the pattern as
  * necessary. For example, all of the following patterns match the
  * same rule:
  *
  * {{{
  * .#.   .#.   #..   ###
  * ..#   #..   #.#   ..#
  * ###   ###   ##.   .#.
  * }}}
  *
  * Suppose the book contained the following two rules:
  *
  * {{{
  * ../.# => ##./#../...
  * .#./..#/### => #..#/..../..../#..#
  * }}}
  *
  * As before, the program begins with this pattern:
  *
  * {{{
  * .#.
  * ..#
  * ###
  * }}}
  *
  * The size of the grid (3) is not divisible by 2, but it is
  * divisible by 3. It divides evenly into a single square; the square
  * matches the second rule, which produces:
  *
  * {{{
  * #..#
  * ....
  * ....
  * #..#
  * }}}
  *
  * The size of this enhanced grid (4) is evenly divisible by 2, so
  * that rule is used. It divides evenly into four squares:
  *
  * {{{
  * #.|.#
  * ..|..
  * --+--
  * ..|..
  * #.|.#
  * }}}
  *
  * Each of these squares matches the same rule (../.# =>
  * ##./#../...), three of which require some flipping and rotation to
  * line up with the rule. The output for the rule is the same in all
  * four cases:
  *
  * {{{
  * ##.|##.
  * #..|#..
  * ...|...
  * ---+---
  * ##.|##.
  * #..|#..
  * ...|...
  * }}}
  *
  * Finally, the squares are joined into a new grid:
  *
  * {{{
  * ##.##.
  * #..#..
  * ......
  * ##.##.
  * #..#..
  * ......
  * }}}
  *
  * Thus, after 2 iterations, the grid contains 12 pixels that are on.
  *
  * How many pixels stay on after 5 iterations?
  */
object FractalArt extends AdventOfCodeDay {
  type P1 = Int
  type P2 = Int

  def runPart1(input: String): Try[P1] =
    Part1.litPixelsAfterIterations(5)(input.trim)

  def runPart2(input: String): Try[P2] =
    Part1.litPixelsAfterIterations(18)(input.trim)

  def verifyPart1Samples(): Unit = {
    import Pixel.{On, Off}
    import Part1.ImageOps

    println("--- parsing ---")
    Seq(
      "../.#" -> """|
                    |..
                    |.#""".stripMargin,
      ".#./..#/###" -> """|
                          |.#.
                          |..#
                          |###""".stripMargin,
      "#..#/..../#..#/.##." -> """|
                                  |#..#
                                  |....
                                  |#..#
                                  |.##.""".stripMargin
    ).map {
      case (input, rendered) =>
        Parser.image.tryToParse(input).map("\n" + _.render).asResult(rendered).show
    }.foreach(println)

    val imgWithOddNumberOfRows = Part1.defaultImage
    val imgWithEvenNumberOfRows = Image(
      Vector(Off, Off, Off, On ),
      Vector(Off, Off, On , Off),
      Vector(Off, Off, On , Off),
      Vector(Off, Off, On , Off))
    val imgWhichShouldBeDivided = Image(
      Vector(On , Off, Off, On ),
      Vector(Off, Off, Off, Off),
      Vector(On , Off, Off, On ),
      Vector(Off, On , On , Off))

    val dividedImage = Image.Divided(
      Vector(
        Vector(
          Image(
            Vector(On , Off),
            Vector(Off, Off)),
          Image(
            Vector(Off, On ),
            Vector(Off, Off))),
        Vector(
          Image(
            Vector(On , Off),
            Vector(Off, On )),
          Image(
            Vector(Off, On ),
            Vector(On , Off)))))

    println("--- transformations (odd number of rows) ---")
    Seq(
      FlipVertical -> """|
                         |###
                         |..#
                         |.#.""".stripMargin,
      FlipHorizontal -> """|
                           |.#.
                           |#..
                           |###""".stripMargin,
      Rotate90 -> """|
                     |#..
                     |#.#
                     |##.""".stripMargin,
      Rotate180 -> """|
                      |###
                      |#..
                      |.#.""".stripMargin,
      Rotate270 -> """|
                      |.##
                      |#.#
                      |..#""".stripMargin
    ).map {
      case (transformation, rendered) =>
        Try("\n" + imgWithOddNumberOfRows.applyTransformation(transformation).render)
          .asResult(rendered)
          .show
    }.foreach(println)

    println("--- transformations (even number of rows) ---")
    Seq(
      FlipVertical -> """|
                         |..#.
                         |..#.
                         |..#.
                         |...#""".stripMargin,
      FlipHorizontal -> """|
                           |#...
                           |.#..
                           |.#..
                           |.#..""".stripMargin,
      Rotate90 -> """|
                     |....
                     |....
                     |###.
                     |...#""".stripMargin,
      Rotate180 -> """|
                      |.#..
                      |.#..
                      |.#..
                      |#...""".stripMargin,
      Rotate270 -> """|
                      |#...
                      |.###
                      |....
                      |....""".stripMargin
    ).map {
      case (transformation, rendered) =>
        Try("\n" + imgWithEvenNumberOfRows.applyTransformation(transformation).render)
          .asResult(rendered)
          .show
    }.foreach(println)

    println("--- splitting image ---")
    Seq(
      imgWithOddNumberOfRows -> imgWithOddNumberOfRows,
      imgWhichShouldBeDivided -> dividedImage
    ).foreach {
      case (input, output) =>
        println(
          Try(input.split).map("\n" + _.render).asResult("\n" + output.render).show
        )
    }

    println("--- merging quartered image ---")
    println(
      Try(dividedImage.flatten)
        .map("\n" + _.render)
        .asResult("\n" + imgWhichShouldBeDivided.render)
        .show)

    println("--- transform using rules ---")
    val resultOfPattern1 =
      Image(
        Vector(On , On , Off),
        Vector(On , Off, Off),
        Vector(Off, Off, Off))
    val resultOfPattern2 =
      Image(
        Vector(On , Off, Off, On ),
        Vector(Off, Off, Off, Off),
        Vector(Off, Off, Off, Off),
        Vector(On , Off, Off, On ))
    val rules = Seq(
      Rule(
        pattern = Image(
          Vector(Off, Off),
          Vector(Off, On )),
        result = resultOfPattern1),
      Rule(
        pattern = Part1.defaultImage,
        result = resultOfPattern2))

    val progression =
      Seq(
        Part1.defaultImage,
        resultOfPattern2,
        Image.Divided(
          Vector(
            Vector(resultOfPattern1,resultOfPattern1),
            Vector(resultOfPattern1,resultOfPattern1))).flatten)

    progression.dropRight(1).zip(progression.drop(1))
      .foreach {
        case (input, output) =>
          println(
            Try(Part1.transformUsing(rules)(input))
              .map("\n" + _.render)
              .asResult("\n" + output.render)
              .show)
      }

    println("--- lit pixels after N iterations ---")
    println(
      Part1
        .litPixelsAfterIterations(2)("""|../.# => ##./#../...
                                        |.#./..#/### => #..#/..../..../#..#""".stripMargin)
        .asResult(12).show)
  }

  def verifyPart2Samples(): Unit = ()
}
