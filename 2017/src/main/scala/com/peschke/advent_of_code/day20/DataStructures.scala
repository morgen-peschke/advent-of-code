package com.peschke.advent_of_code
package day20

case class Point(x: Int, y: Int, z: Int)
case class Velocity(x: Int, y: Int, z: Int)
case class Acceleration(x: Int, y: Int, z: Int)

case class Particle(point: Point, velocity: Velocity, acceleration: Acceleration)

object Parser {
  val IgnoreWhiteSpace = fastparse.WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import IgnoreWhiteSpace._

  val sign: P[Boolean] = P("-".!.?).map(_.isDefined)

  val digits: P[Int] = P(CharIn('0' to '9').rep.!).map(_.toInt)
  val number: P[Int] = (sign ~ digits).map {
    case (isNegative, number) => if (isNegative) -number else number
  }

  val triple: P[(Int, Int, Int)] = number ~/ "," ~/ number ~/ "," ~/ number

  val point       : P[Point]        = P("p=<" ~/ NoCut(triple) ~/ ">").map(Point.tupled)
  val velocity    : P[Velocity]     = P("v=<" ~/ NoCut(triple) ~/ ">").map(Velocity.tupled)
  val acceleration: P[Acceleration] = P("a=<" ~/ NoCut(triple) ~/ ">").map(Acceleration.tupled)

  val particle: P[Particle] =
    P(point ~/ "," ~/ velocity ~/ "," ~/ acceleration).map(Particle.tupled)
}
