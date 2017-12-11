package com.peschke.advent_of_code.day7

import cats.data.NonEmptyList

class RecursiveCircusFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"Recursive Circus failed on input:\n$input", cause)

case class Name(n: String)

case class ProgramInfo(name: Name, weight: Int, supporting: List[Name])

sealed trait Program {
  val name: Name
  val weight: Int
}
object Program {
  case class DiscHolder(name: Name, weight: Int, holding: NonEmptyList[Program])
      extends Program

  case class BalanceHelper(name: Name, weight: Int) extends Program
}

case class UnbalancedProgram(name: Name, weight: Int, delta: Int)
