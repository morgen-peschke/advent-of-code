package com.peschke.advent_of_code
package day14

import scala.util.Try
import com.peschke.advent_of_code.day10.Part2.hash
import com.peschke.advent_of_code.day10.RenderedHash

import cats.instances.try_._
import cats.instances.vector._
import cats.syntax.traverse._

object Part1 {
  implicit class HashOps(val rh: RenderedHash) extends AnyVal {
    def asBigInt: Try[BigInt] = Try(BigInt(rh.hash, 16))
  }

  implicit class RowOps(val row: Row) extends AnyVal {
    def usedSquares: Int = row.squares.bitCount
  }

  implicit class GridOps(val grid: Grid) extends AnyVal {
    def usedSquares: Int = grid.rows.map(_.usedSquares).sum
  }

  def convertToGrid(input: String): Try[Grid] =
    (0 to 127).toVector
      .traverse { row =>
        for {
          renderedHash <- hash(s"$input-$row")
          squares <- renderedHash.asBigInt
        } yield Row(squares)
      }
      .map(Grid(_))

  def usedSquares(input: String): Try[Int] = convertToGrid(input).map(_.usedSquares)
}
