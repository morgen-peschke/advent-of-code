package com.peschke.advent_of_code.day3

import scala.util.{Failure, Try}

import com.peschke.advent_of_code.AdventOfCodeDay._

object Part1 {
  case class Level(n: Int, remainder: Double) {
    def combined: Double = n.toDouble + remainder
    /**
      * Corner       | move   | Offset
      * -------------+--------+------------
      * 1  = 1       |   r    | 0
      * 2  = 2 + 1   |   u    | l
      * 3  = 2 + 1   |   l    | d l
      * 5  = 3 + 2   |   d    | d r
      * 7  = 5 + 2   |   r    | u r
      * 10 = 7 + 3   |   u    | u l l
      * 13 = 10 + 3  |   l    | d d l l
      * 17 = 13 + 4  |   d    | d d r r
      * 21 = 17 + 4  |   r    | u u r r
      * 26 = 21 + 5  |   u    | u u l l l
      * 31 = 26 + 5  |   l    | d d d l l l
      *
      * Upper Left Corner
      * --------------+-----------+--------------------------------------+-----------+-----------
      * 5   = 1  + 4  | 4(1) +  1 |                            4(1) +  1 | 1 + 4(1)  | 1 + 4(n^2)
      * 17  = 5  + 12 | 4(3) +  5 |                     4(3) + 4(1) +  1 | 1 + 4(4)  | 1 + 4(n^2)
      * 37  = 17 + 20 | 4(5) + 17 |              4(5) + 4(3) + 4(1) +  1 | 1 + 4(9)  | 1 + 4(n^2)
      * 65  = 37 + 28 | 4(7) + 37 |        4(7) +4(5) + 4(3) + 4(1) +  1 | 1 + 4(16) | 1 + 4(n^2)
      * 101 = 65 + 36 | 4(9) + 65 | 4(9) + 4(7) +4(5) + 4(3) + 4(1) +  1 | 1 + 4(25) | 1 + 4(n^2)
      */
    def toSquare: Square = Square(1 + (4 * Math.pow(combined, 2)).floor.toInt)
  }

  case class Square(id: Int) {
    def toLevel: Level = {
      val rawLevel = Math.sqrt( (id - 1).toDouble / 4 )
      val n = rawLevel.floor.toInt
      Level(n, rawLevel - n)
    }

    def toCorner(square: Square): Option[Corner] =
      if (square.id == 0) Some(Corner.LowerLeft(square))
      else Option(square.toLevel.remainder).collect {
        case 0.00 => Corner.UpperLeft(square)
        case 0.25 => Corner.LowerLeft(square)
        case 0.50 => Corner.LowerRight(square)
        case 0.75 => Corner.UpperRight(square)
      }
  }

  sealed trait Corner {
    def square: Square
  }
  object Corner {
    case class UpperLeft(val square: Square) extends Corner
    case class LowerLeft(val square: Square) extends Corner
    case class LowerRight(val square: Square) extends Corner
    case class UpperRight(val square: Square) extends Corner
  }

  case class Anchor(corner: Corner, offsetFromPort: Offset) {
    def continue(n: Steps): Offset = {
      val extraOffset =
        corner match {
          case Corner.UpperLeft(_) => Offset(vertical = n)
          case Corner.LowerLeft(_) => Offset(horizontal = n)
          case Corner.LowerRight(_) => Offset(vertical = -n)
          case Corner.UpperRight(_) => Offset(horizontal = -n)
        }
      offsetFromPort - extraOffset
    }
  }

  def previousAnchor(square: Square): Anchor =
    if (square.id == 0) Anchor(Corner.LowerLeft(square), Offset())
    else {
      val level = square.toLevel

      val corner =
        if (level.remainder < 0.25)
          Corner.UpperLeft(level.copy(remainder = 0.0).toSquare)
        else if (level.remainder < 0.50)
          Corner.LowerLeft(level.copy(remainder = 0.25).toSquare)
        else if (level.remainder < 0.75)
          Corner.LowerRight(level.copy(remainder = 0.50).toSquare)
        else
          Corner.UpperRight(level.copy(remainder = 0.75).toSquare)

      val steps = Steps(level.n)
      val offset = corner match {
        case Corner.UpperLeft(_) =>
          Offset(
            vertical = steps,
            horizontal = steps)
        case Corner.LowerLeft(_) =>
          Offset(
            vertical = -steps,
            horizontal = steps)
        case Corner.LowerRight(_) =>
          Offset(
            vertical = -steps,
            horizontal = steps + 1)
        case Corner.UpperRight(_) =>
          Offset(
            vertical = steps,
            horizontal = -steps)
      }
      Anchor(corner, offset)
    }

  def calculateOffset(square: Square): Offset = {
    val anchor = previousAnchor(square)
    anchor.continue(Steps(square.id - anchor.corner.square.id))
  }

  def spiralMemory(input: String): Try[Steps] =
    SpiralMemory.parse(input).map { squareId =>
      calculateOffset(Square(squareId)).distance
    }.wrapFailure(throwable => Failure(new SpiralMemoryFailure(input, throwable)))
}
