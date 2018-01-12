package com.peschke.advent_of_code
package day17

import scala.util.Try

object Part1 {
  implicit class GrowingSpinLockOps(val gpl: GrowingSpinLock) extends AnyVal {
    def nextPosition(stepSize: StepSize): Position =
      Position((gpl.position.value + stepSize.value) % gpl.size.value)

    def appendAfter(position: Position)(value: Int): GrowingSpinLock =
      GrowingSpinLock(
        Position(position.value + 1),
        gpl.elements.patch(position.value + 1, Vector(value), 0))
  }

  def parse(input: String): Try[StepSize] = Try(StepSize(input.toInt))

  def walkThroughSpinLock(steps: Int, stepSize: StepSize): GrowingSpinLock =
    (1 to steps).foldLeft(GrowingSpinLock.initialize) { (spinlock, step) =>
      spinlock.appendAfter(spinlock.nextPosition(stepSize))(step)
    }

  def shortCircuit(input: String): Try[Int] =
    parse(input)
      .map(walkThroughSpinLock(2017, _))
      .map { gpl =>
        gpl.elements(gpl.position.value + 1)
      }
}
