package com.peschke.advent_of_code
package day8

object Part2 {
  import Part1.{Memory, ComparisonOps, OperationOps}

  def eval(is: List[Instruction]): Stream[Memory] = {
    def loop(state: Memory, rest: List[Instruction]): Stream[Memory] =
      rest match {
        case Nil => Stream.empty[Memory]
        case Instruction(operation, comparison) :: tail if comparison.check(state) =>
          val nextState = operation.applyTo(state)
          nextState #:: loop(nextState, tail)
        case _ :: tail => state #:: loop(state, tail)
      }
    Memory.empty #:: loop(Memory.empty, is)
  }
}
