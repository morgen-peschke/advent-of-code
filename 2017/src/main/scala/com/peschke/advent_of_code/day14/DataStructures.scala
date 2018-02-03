package com.peschke.advent_of_code
package day14

case class Row(squares: BigInt) {
  require(
    squares.bitLength <= 128,
    s"Row length cannot exceed 128 squares, was ${squares.bitLength}")
}

case class Grid(rows: Vector[Row])

case class Coord(row: Int, col: Int)

case class Bits(set: Set[Coord])
