package com.peschke.advent_of_code
package day11

sealed trait Direction
object Direction {
  case object North     extends Direction
  case object NorthEast extends Direction
  case object SouthEast extends Direction
  case object South     extends Direction
  case object SouthWest extends Direction
  case object NorthWest extends Direction

  def parse(s: String): Direction = s.toLowerCase match {
    case "n"  => North
    case "ne" => NorthEast
    case "se" => SouthEast
    case "s"  => South
    case "sw" => SouthWest
    case "nw" => NorthWest
    case _ => throw new IllegalArgumentException(s"No direction matches <$s>")
  }
}

case class CubeCoord[N](q: N, r: N, s: N)(implicit val numeric: Numeric[N]) {
  import numeric._

  require(
    q + r + s == numeric.zero,
    s"q + r + s = 0 invariant violated by $q + $r + $s = ${q + r + s}")

  def + (other: CubeCoord[N]): CubeCoord[N] =
    CubeCoord(
      q = this.q + other.q,
      r = this.r + other.r,
      s = this.s + other.s)

  def distanceTo (other: CubeCoord[N]): N =
    Seq(
      (this.q - other.q).abs,
      (this.r - other.r).abs,
      (this.s - other.s).abs
    ).max

  def move(direction: Direction): CubeCoord[N] =
    this + (direction match {
      case Direction.North     => CubeCoord[N](fromInt(0),  fromInt(1),  fromInt(-1))
      case Direction.NorthEast => CubeCoord[N](fromInt(1),  fromInt(0),  fromInt(-1))
      case Direction.SouthEast => CubeCoord[N](fromInt(1),  fromInt(-1), fromInt(0))
      case Direction.South     => CubeCoord[N](fromInt(0),  fromInt(-1), fromInt(1))
      case Direction.SouthWest => CubeCoord[N](fromInt(-1), fromInt(0),  fromInt(1))
      case Direction.NorthWest => CubeCoord[N](fromInt(-1), fromInt(1),  fromInt(0))
    })
}

object CubeCoord {
  def origin[N](implicit n: Numeric[N]): CubeCoord[N] = CubeCoord[N](n.zero, n.zero, n.zero)
}
