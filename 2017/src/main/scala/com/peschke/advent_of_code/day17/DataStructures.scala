package com.peschke.advent_of_code
package day17

case class StepSize(value: Int) extends AnyVal
case class Position(value: Int) extends AnyVal
case class Size(value: Int)

case class GrowingSpinLock(position: Position, elements: Vector[Int]) {
  def size: Size = Size(elements.size)
}

object GrowingSpinLock {
  def initialize: GrowingSpinLock =
    GrowingSpinLock(Position(0), Vector(0))
}

case class SparseSpinLock(position: Position, elementAfterZero: Int, size: Size)

object SparseSpinLock {
  def initialize: SparseSpinLock =
    SparseSpinLock(Position(0), 0, Size(1))
}
