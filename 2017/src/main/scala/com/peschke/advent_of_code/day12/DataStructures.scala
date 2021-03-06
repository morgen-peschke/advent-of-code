package com.peschke.advent_of_code
package day12

case class Program(label: String)
object Program {
  implicit val ordering: Ordering[Program] = Ordering.by(_.label)
}

case class Pipe(p0: Program, p1: Program)

object Pipe {
  def apply(p0: Program, p1: Program): Pipe = {
    val (start :: end :: Nil) = List(p0, p1).sorted
    new Pipe(start, end)
  }

  implicit val ordering: Ordering[Pipe] = Ordering.by(_.p0)
}

object Parser {
  import fastparse.all._

  val program: P[Program] = P(CharIn('0' to '9').rep.!.map(Program(_)))
  val sep: P[Unit] = P(" <-> ")
  val programAndConnections: P[(Program, Seq[Program])] =
    P(program ~/ sep ~/ program.rep(sep = ", "))
}
