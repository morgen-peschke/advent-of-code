package com.peschke.advent_of_code
package day16

import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.either._

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefType.applyRef
import eu.timepit.refined.numeric._

case class Program(name: Program.Name)
object Program {
  type Name = Char Refined Interval.Closed[W.`'a'`.T, W.`'p'`.T]
}

sealed trait DanceMove {
  def render: String
}
case class Spin(count: Int) extends DanceMove {
  def render: String = s"s$count"
}
case class Exchange(a: Exchange.Position, b: Exchange.Position) extends DanceMove {
  def render: String = s"x$a/$b"
}
case class Partner(a: Program.Name, b: Program.Name) extends DanceMove {
  def render: String = s"p$a/$b"
}

object Exchange {
  type Position = Int Refined Interval.Closed[W.`0`.T, W.`15`.T]
}

case class DanceLine(programs: Vector[Program]) {
  def render: String = programs.map(_.name).mkString("[", "", "]")
}
object DanceLine {
  def ofLength(l: Int Refined Positive): Either[String,DanceLine] =
    ('a' until ('a' + l.value).toChar)
      .toVector
      .traverse(c => applyRef[Program.Name](c))
      .map { names =>
        DanceLine(names.map(Program(_)))
      }
}
