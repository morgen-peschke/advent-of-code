package com.peschke.advent_of_code

package object day22 {
  implicit class PositionConstructor(val facing: Facing) extends AnyVal {
    def at(row: Int, col: Int): Position = Position(facing, Node(row, col))
  }
}
