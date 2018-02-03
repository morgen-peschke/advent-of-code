package com.peschke.advent_of_code
package day13

sealed trait Direction {
  def render: String = this match {
    case Up => "↑"
    case Down => "↓"
  }
}
case object Up extends Direction
case object Down extends Direction

case class Scanner(range: Int, location: Int, direction: Direction) {
  def render(row: Int, packetInLayer: Boolean): String = {
    def scannerMark =
      if (row == location) direction.render else " "

    if (row == 0 && packetInLayer) s"($scannerMark)"
    else if ((0 to range).contains(row)) s"[$scannerMark]"
    else "   "
  }
}

case class Layer(packetInLayer: Boolean, maybeScanner: Option[Scanner]) {
  def render(row: Int): String = {
    def empty: String =
      if (row == 0) {
        if (packetInLayer) "(.)" else "..."
      }
      else "   "

    maybeScanner.fold(empty)(_.render(row, packetInLayer))
  }
}

object Layer {
  def empty: Layer = Layer(false, None)
  def scanning(scanner: Scanner): Layer = Layer(false, Some(scanner))
}

case class Firewall(layers: Vector[Layer]) {
  override def toString: String = {
    val maxRow: Int = layers.flatMap(_.maybeScanner).map(_.range) match {
      case Vector() => 0
      case ranges => ranges.max
    }
    val titleRow = layers.indices.map(i => " %-2s".format(i)).mkString(" ")
    val depthRows =
      (0 to maxRow)
        .toVector
        .map { row =>
          layers.map(_.render(row)).mkString(" ")
        }

    (titleRow +: depthRows).mkString("\n")
  }
}

object Firewall {
  def apply(l: Layer, ls: Layer*): Firewall =
    Firewall(l +: ls.toVector)
}

case class Caught(depth: Int, scanner: Scanner)

case class Severity(level: Int) extends AnyVal
case class Delay(picoseconds: Int) extends AnyVal
