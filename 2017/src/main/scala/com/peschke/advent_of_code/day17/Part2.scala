package com.peschke.advent_of_code
package day17

import scala.util.Try

object Part2 {
  implicit class SizeOps(val size: Size) extends AnyVal {
    def increment: Size = Size(size.value + 1)
  }

  implicit class SparseSpinLockOps(val ssl: SparseSpinLock) extends AnyVal {
    def nextPosition(stepSize: StepSize): Position =
      Position((ssl.position.value + stepSize.value) % ssl.size.value)
  }

  def walkThroughSpinLock(steps: Int, stepSize: StepSize): Int =
    (1 to steps).foldLeft(SparseSpinLock.initialize) { (spinlock, step) =>
      spinlock.nextPosition(stepSize) match {
        case Position(0) => SparseSpinLock(Position(1), step, spinlock.size.increment)
        case Position(i) =>
          spinlock.copy(
            position = Position(i + 1),
            size = spinlock.size.increment)
      }
    }.elementAfterZero

  def shortCircuit(input: String): Try[Int] =
    Part1.parse(input).map(walkThroughSpinLock(50000000, _))
}
