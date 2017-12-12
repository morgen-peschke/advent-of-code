package com.peschke.advent_of_code
package day8

object Part1 {
  type Memory = Map[Register, Amount]
  object Memory {
    val empty: Memory = Map.empty[Register, Amount]
    def apply(values: (Register, Amount)*): Memory = Map(values:_*)
  }

  implicit class MemoryOps(val m: Memory) extends AnyVal {
    def at(r: Register): Amount = m.getOrElse(r, Amount(0))
  }

  implicit class ComparisonOps(val conditional: Conditional) extends AnyVal {
    import Conditional.Comparison._

    def check(memory: Memory): Boolean = {
      val lhs = memory.at(conditional.register).value
      val rhs = conditional.amount.value
      def applyCmp(c: Conditional.Comparison): Boolean = c match {
        case NotEqualTo  => lhs != rhs
        case EqualTo     => lhs == rhs
        case LessThan    => lhs <  rhs
        case GreaterThan => lhs >  rhs
        case c1 Or c2    => applyCmp(c1) || applyCmp(c2)
      }
      applyCmp(conditional.cmp)
    }
  }

  implicit class OperationOps(val operation: Operation) extends AnyVal {
    import Operation.OpCode._

    def applyTo(memory: Memory): Memory = {
      val lhs = memory.at(operation.register).value
      val rhs = operation.amount.value
      def applyOp(o: Operation.OpCode): Amount = o match {
        case Increment => Amount(lhs + rhs)
        case Decrement => Amount(lhs - rhs)
      }
      memory + (operation.register -> applyOp(operation.opCode))
    }
  }

  def eval(is: List[Instruction]): Memory = {
    def loop(state: Memory, rest: List[Instruction]): Memory =
      rest match {
        case Nil => state
        case Instruction(operation, comparison) :: tail if comparison.check(state) =>
          loop(operation.applyTo(state), tail)
        case _ :: tail => loop(state, tail)
      }
    loop(Memory.empty, is)
  }
}
