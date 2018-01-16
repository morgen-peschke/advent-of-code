package com.peschke.advent_of_code
package day19

sealed trait Tile extends Product with Serializable {
  def render: String = this match {
    case X => " "
    case ╬ => "+"
    case ═ => "-"
    case ║ => "|"
    case C(v) => s"$v"
  }
}
case object X extends Tile
case object ╬ extends Tile
case object ═ extends Tile
case object ║ extends Tile
case class C(value: Char) extends Tile

sealed trait Direction extends Product with Serializable
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class Coord(row: Int, column: Int) {
  override def toString: String = s"[$row, $column]"
}

case class RoutingDiagram(locations: Map[Coord, Tile], start: Coord)

case class Neighborhood(
  nw: Tile, n     : Tile, ne: Tile,
  w : Tile, origin: Tile, e : Tile,
  sw: Tile, s     : Tile, se: Tile) {
  def render: String =
    Seq(
      Seq(nw.render, n.render     , ne.render),
      Seq(w.render , origin.render, e.render ),
      Seq(sw.render, s.render     , se.render)
    ).map(_.mkString).mkString("\n")
}

class MalformedRoutingDiagramException(
  location: Coord,
  direction: Direction,
  neighborhood: Neighborhood,
  desc: String) extends RuntimeException(
  s"Unable to interpret map going $direction at $location: $desc\n${neighborhood.render}"
)
