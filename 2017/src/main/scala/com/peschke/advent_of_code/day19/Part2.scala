package com.peschke.advent_of_code
package day19

object Part2 {
  import Part1.{RoutingDiagramOps, CoordOps}

  def foldLeftOverRoutingDiagram[T](
    routingDiagram: RoutingDiagram,
    zero: T)(
    f: (T, Tile) => T
  ): T = {
    @scala.annotation.tailrec
    def loop(location: Coord, direction: Direction, accum: T): T = {
      def accumulate = f(accum, routingDiagram.at(location))

      val neighborhood = routingDiagram.neighborhood(location)

      def malformedRoutingDiagramException(desc: String) =
        new MalformedRoutingDiagramException(location, direction, neighborhood, desc)

      (direction, neighborhood) match {
        case
            (_, Neighborhood(
              _, _, _,
              _, X, _,
              _, _, _
            ))
            => accum
        case
            (_, Neighborhood(
              _, _       , _,
              _, C(_), _,
              _, _       , _
            )) => loop(location.step(direction), direction, accumulate)
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
            )) => loop(location.step(East), East, accumulate)
        case
            (North | South, Neighborhood(
              _       , _, _,
              ═ | C(_), ╬, _,
              _       , _, _
            )) => loop(location.step(West), West, accumulate)
        case
            (East | West, Neighborhood(
              _, ║ | C(_), _,
              _, ╬       , _,
              _, _       , _
            )) => loop(location.step(North), North, accumulate)
        case
            (East | West, Neighborhood(
              _, _       , _,
              _, ╬       , _,
              _, ║ | C(_), _
            )) => loop(location.step(South), South, accumulate)
        case _ => loop(location.step(direction), direction, accumulate)
      }
    }

    loop(routingDiagram.start, South, zero)
  }

  implicit class RoutingDiagramFoldLeft(val rd: RoutingDiagram) extends AnyVal {
    def foldLeft[T](zero: T)(f: (T, Tile) => T): T = foldLeftOverRoutingDiagram(rd, zero)(f)
  }

  def stepCount(input: String): scala.util.Try[Int] =
    Part1.parse(input)
      .map(_.foldLeft(0) { (sum, _) => sum + 1
      })
}
