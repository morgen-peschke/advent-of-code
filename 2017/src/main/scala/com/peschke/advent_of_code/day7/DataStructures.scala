package com.peschke.advent_of_code
package day7

import cats.Eq
import cats.instances.string._
import cats.data.NonEmptyList

case class Name(n: String)
object Name {
  implicit val eq: Eq[Name] = Eq.by(_.n)
}

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
