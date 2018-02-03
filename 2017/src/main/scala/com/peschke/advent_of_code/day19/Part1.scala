package com.peschke.advent_of_code
package day19

import scala.util.Try

object Part1 {
  def parse(input: String): Try[RoutingDiagram] = Try {
    val locationLines =
      input
        .split('\n')
        .toVector
        .zipWithIndex
        .map {
          case (rawLine, rowIndex) =>
            rawLine
              .toVector
              .zipWithIndex
              .collect {
                case ('|', columnIndex) => Coord(rowIndex, columnIndex) -> ║
                case ('-', columnIndex) => Coord(rowIndex, columnIndex) -> ═
                case ('+', columnIndex) => Coord(rowIndex, columnIndex) -> ╬
                case (c, columnIndex) if c.isLetter =>
                  Coord(rowIndex, columnIndex) -> C(c)

                case (c, columnIndex) if c != ' ' && c.toInt != 0 =>
                  throw new IllegalArgumentException(
                    s"Unexpected character ${c.toInt} '$c' at $rowIndex, $columnIndex")
              }
        }
    val locations = locationLines.flatten.toMap
    val startCoord =
      locationLines
        .headOption
        .toVector
        .flatten
        .collect {
          case (coord @ Coord(0, _), _) => coord
        } match {
          case Vector(coord) => coord
          case Vector() => throw new IllegalArgumentException(
            "Unable to locate start location: " +
              "nothing appears to be touching the top of the diagram")
          case multipleCoords => throw new IllegalArgumentException(
            "Unable to locate start location: " +
              "multiple lines appear to be touching the top of the diagram " +
              multipleCoords.mkString("<", ", ", ">"))
        }
    RoutingDiagram(locations, startCoord)
  }

  implicit class RoutingDiagramOps(val rd: RoutingDiagram) extends AnyVal {
    def at(coord: Coord): Tile = rd.locations.getOrElse(coord, X)

    def neighborhood(c: Coord): Neighborhood =
      Neighborhood(
        nw = at(c.step(North, West)), n      = at(c.step(North)), ne = at(c.step(North, East)),
        w  = at(c.step(West       )), origin = at(c)            , e  = at(c.step(East       )),
        sw = at(c.step(South, West)), s      = at(c.step(South)), se = at(c.step(South, East)))
  }

  implicit class CoordOps(val c: Coord) extends AnyVal {
    def step(d: Direction): Coord = d match {
      case North => c.copy(row = c.row - 1)
      case South => c.copy(row = c.row + 1)
      case West  => c.copy(column = c.column - 1)
      case East  => c.copy(column = c.column + 1)
    }

    def step(d1: Direction, d2: Direction): Coord = c.step(d1).step(d2)
  }

  def navigate(routingDiagram: RoutingDiagram): Try[String] = {
    @scala.annotation.tailrec
    def loop(location: Coord, direction: Direction, letters: Vector[Char]): String = {
      val neighborhood = routingDiagram.neighborhood(location)
      def malformedRoutingDiagramException(desc: String) =
        new MalformedRoutingDiagramException(location, direction, neighborhood, desc)

      (direction, neighborhood) match {
        case
            (_, Neighborhood(
              _, _   , _,
              _, C(l), _,
              _, _   , _
            )) => loop(location.step(direction), direction, letters :+ l)
        case
            (_, Neighborhood(
              _, _, _,
              _, X, _,
              _, _, _
            ))
            => letters.mkString
        case
            (North | South, Neighborhood(
              _, _, _,
              ═, ╬, ═,
              _, _, _
            )) |
            (East | West, Neighborhood(
              _, ║, _,
              _, ╬, _,
              _, ║, _
            )) => throw malformedRoutingDiagramException("Ambiguous turn")
        case
            (North, Neighborhood(
              _, ║, _,
              _, ╬, _,
              _, _, _
            )) |
            (South, Neighborhood(
              _, _, _,
              _, ╬, _,
              _, ║, _
            )) |
            (East, Neighborhood(
              _, _, _,
              _, ╬, ═,
              _, _, _
            )) |
            (West, Neighborhood(
              _, _, _,
              ═, ╬, _,
              _, _, _
            )) => throw malformedRoutingDiagramException("Turn appears to go straight")
        case
            (North | South, Neighborhood(
              _, _, _       ,
              _, ╬, ═ | C(_),
              _, _, _
            )) => loop(location.step(East), East, letters)
        case
            (North | South, Neighborhood(
              _       , _, _,
              ═ | C(_), ╬, _,
              _       , _, _
            )) => loop(location.step(West), West, letters)
        case
            (East | West, Neighborhood(
              _, ║ | C(_), _,
              _, ╬       , _,
              _, _       , _
            )) => loop(location.step(North), North, letters)
        case
            (East | West, Neighborhood(
              _, _       , _,
              _, ╬       , _,
              _, ║ | C(_), _
            )) => loop(location.step(South), South, letters)
        case _ => loop(location.step(direction), direction, letters)
      }
    }
    Try(loop(routingDiagram.start, South, Vector.empty))
  }
}
