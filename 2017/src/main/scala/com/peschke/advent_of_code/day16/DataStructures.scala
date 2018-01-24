package com.peschke.advent_of_code
package day16

import cats.syntax.traverse._
import cats.syntax.either._
import cats.instances.vector._
import cats.instances.either._

case class Program(name: Program.Name)
object Program {
  case class Name(value: Char) extends AnyVal
  object Name {
    def apply(v: Char): Either[String, Name] =
      if (('a' to 'p').contains(v)) new Name(v).asRight
      else s"'$v' is out of bounds for Name".asLeft
  }
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
  type Position = Int
}

case class DanceLine(programs: Vector[Program]) {
  def render: String = programs.map(_.name).mkString("[", "", "]")
}
object DanceLine {
  type EitherOrError[A] = Either[String, A]
  def ofLength(l: Int): Either[String,DanceLine] =
    ('a' until ('a' + l).toChar)
      .toVector
      .traverse[EitherOrError, Program.Name](c => Program.Name(c))
      .map { names =>
        DanceLine(names.map(Program(_)))
      }
}
