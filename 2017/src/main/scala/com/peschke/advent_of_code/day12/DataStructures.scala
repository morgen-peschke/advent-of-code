package com.peschke.advent_of_code
package day12

class DigitalPlumberFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"DigitalPlumber failed on input:\n$input", cause)

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
  import fastparse.core.Parsed

  val program: P[Program] = P(CharIn('0' to '9').rep.!.map(Program(_)))
  val sep: P[Unit] = P(" <-> ")
  val programAndConnections: P[(Program, Seq[Program])] =
    P(program ~/ sep ~/ program.rep(sep = ", "))

  def parse(input: String): (Program, Seq[Program]) =
    programAndConnections.parse(input) match {
      case Parsed.Success(value, _) => value
      case f @ Parsed.Failure(_, _, _) =>
        throw new IllegalArgumentException(s"Unable to parse <$input>\n${f.msg}")
    }
}
