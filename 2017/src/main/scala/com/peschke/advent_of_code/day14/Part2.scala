package com.peschke.advent_of_code
package day14

import scala.util.Try

object Part2 {
  val Zero: BigInt = BigInt(0)

  implicit class RowOps(val row: Row) extends AnyVal {
    def usedSquares: Set[Int] = {
      def loop(squares: BigInt, accum: Set[Int]): Set[Int] =
        if (squares == Zero) accum
        else {
          val lowestSetBit = squares.lowestSetBit
          loop(squares.clearBit(lowestSetBit), accum + lowestSetBit)
        }
      loop(row.squares, Set.empty)
    }
  }

  implicit class GridOps(val grid: Grid) extends AnyVal {
    def asBits: Bits =
      Bits(
        grid
          .rows
          .zipWithIndex
          .toSet[(Row, Int)]
          .flatMap {
            case (row, index) => row.usedSquares.map(Coord(index, _))
          })
  }

  implicit class CoordOps(val coord: Coord) extends AnyVal {
    def isValid: Boolean =
      coord.row >= 0 &&
        coord.row <= 127 &&
        coord.col >= 0 &&
        coord.col <= 127

    def neighboors: Set[Coord] =
      Set(
        Coord(coord.row - 1, coord.col),
        Coord(coord.row + 1, coord.col),
        Coord(coord.row, coord.col - 1),
        Coord(coord.row, coord.col + 1)
      ).filter(_.isValid)
  }

  implicit class BitOps(val bits: Bits) extends AnyVal {
    def removeRegion: Option[Bits] =
      bits.set.headOption.map { coord =>
        def loop(toRemove: Set[Coord], left: Set[Coord]): Bits =
          if (toRemove.isEmpty) Bits(left)
          else {
            val removed = left diff toRemove
            loop(toRemove.flatMap(_.neighboors).filter(removed), removed)
          }
        loop(Set(coord), bits.set)
      }
  }

  def regions(input: String): Try[Int] =
    Part1.convertToGrid(input).map { grid =>
      Iterator.iterate(grid.asBits.removeRegion)(_.flatMap(_.removeRegion))
        .takeWhile(_.nonEmpty)
        .length
    }
}
