package com.peschke.advent_of_code.day3

import scala.util.{Failure, Try}

import com.peschke.advent_of_code.AdventOfCodeDay._

import Steps.syntax._

object Part2 {
  case class Location(square: Square, offset: Offset) {
    def render(numberWidth: Int): String =
      s"${square.render(numberWidth)} @ ${offset.render(numberWidth)}"

    def toCell[T](data: T): Cell[T] = offset -> SquareWithData(square, data)
  }
  object Location {
    val port: Location = Location(Square.Port, Offset.zero)
  }

  sealed trait Direction
  object Up extends Direction
  object Down extends Direction
  object Left extends Direction
  object Right extends Direction

  implicit class ShiftOffsetOpts(val o: Offset) extends AnyVal {
    def go(d: Direction): Offset = d match {
      case Up => o.up
      case Down => o.down
      case Left => o.left
      case Right => o.right
    }

    def neighbors: Seq[Offset] = {
      val north = o.up
      val south = o.down
      val east = o.right
      val west = o.left
      val ne = north.right
      val nw = north.left
      val se = south.right
      val sw = south.left
      Seq(north, south, east, west, ne, nw, se, sw)
    }
  }

  sealed abstract class Square(val direction: Direction) {
    val id: Int
    def nextId: Int = id + 1

    def turn: Square = (id match {
      case 1 => Square.BottomRight.apply _
      case 2 => Square.TopRight.apply _
      case _ => this match {
        case Square.Port           => Square.BottomRight.apply _
        case Square.BottomRight(_) => Square.RightSide.apply _
        case Square.RightSide(_)   => Square.TopRight.apply _
        case Square.TopRight(_)    => Square.Top.apply _
        case Square.Top(_)         => Square.TopLeft.apply _
        case Square.TopLeft(_)     => Square.LeftSide.apply _
        case Square.LeftSide(_)    => Square.BottomLeft.apply _
        case Square.BottomLeft(_)  => Square.Bottom.apply _
        case Square.Bottom(_)      => Square.BottomRight.apply _
      }
    })(nextId)

    def arrow: String = this match {
      case Square.Port           => "⦿"
      case Square.BottomRight(_) => "┘"
      case Square.RightSide(_)   => "┤"
      case Square.TopRight(_)    => "┐"
      case Square.Top(_)         => "┬"
      case Square.TopLeft(_)     => "┌"
      case Square.LeftSide(_)    => "├"
      case Square.BottomLeft(_)  => "└"
      case Square.Bottom(_)      => "┴"
    }

    def render(numberWidth: Int): String = s"${id.toString.padTo(numberWidth, ' ')} : $arrow"
  }

  object Square {
    sealed trait Side extends Square {
      def continue: Square = this match {
        case s @ Square.Top(_)       => s.copy(id = nextId)
        case s @ Square.Bottom(_)    => s.copy(id = nextId)
        case s @ Square.LeftSide(_)  => s.copy(id = nextId)
        case s @ Square.RightSide(_) => s.copy(id = nextId)
      }
    }

    case object Port extends Square(Right) {
      val id: Int = 1
    }

    case class BottomRight(id: Int) extends Square(Up)
    case class RightSide  (id: Int) extends Square(Up) with Side
    case class TopRight   (id: Int) extends Square(Left)
    case class Top        (id: Int) extends Square(Left) with Side
    case class TopLeft    (id: Int) extends Square(Down)
    case class LeftSide   (id: Int) extends Square(Down) with Side
    case class BottomLeft (id: Int) extends Square(Right)
    case class Bottom     (id: Int) extends Square(Right) with Side
  }

  case class SquareWithData[T](square: Square, data: T)

  type Cell[T] = (Offset, SquareWithData[T])
  type Memory[T] = Map[Offset, SquareWithData[T]]

  object Memory {
    def empty[T]: Memory[T] = Map.empty[Offset, SquareWithData[T]]
  }

  implicit class CellOpts[T](val cell: Cell[T]) extends AnyVal {
    def offset: Offset = cell._1
    def square: Square = cell._2.square
    def data: T = cell._2.data
    def asLocation: Location = Location(cell.square, cell.offset)
  }

  implicit class MemoryOpts[T](val memory: Memory[T]) extends AnyVal {
    def show(empty: String)(renderCell: Cell[T] => String): String =
      if (memory.isEmpty) ""
      else {
        val locations = memory.toSeq
        val rowOffset = locations.map(_.offset.vertical).min.abs.n
        val colOffset = locations.map(_.offset.horizontal).min.abs.n
        val maxRow = (locations.map(_.offset.vertical).max + rowOffset).n
        val maxCol = (locations.map(_.offset.horizontal).max + colOffset).n
        val rendered =
          (0 to maxRow).map { r =>
            (0 to maxCol).map { c =>
              val offset = Offset(
                vertical = (r - rowOffset).steps,
                horizontal = (c - colOffset).steps)

              memory.get(offset).map(offset -> _).fold(empty)(renderCell)
            }
          }
        val maxWidth = rendered.flatten.map(_.length).max

        rendered.map { row =>
          row.map(_.reverse.padTo(maxWidth, ' ').reverse).mkString(" ")
        }.mkString("\n")
      }
  }

  def allocator: Stream[Location] = {
    def loop(current: Location, leftOnRow: Int, rowLength: Int): Stream[Location] = {
      def turn: Location =
        Location(
          square = current.square.turn,
          offset = current.offset.go(current.square.direction))

      def advance(s: Square.Side): Location =
        Location(
          square = if (leftOnRow == 1) s.turn else s.continue,
          offset = current.offset.go(s.direction))

      current.square match {
        case Square.Port | Square.BottomRight(_) | Square.TopLeft(_) =>
          val next = turn
          next #:: loop(next, leftOnRow = rowLength, rowLength = rowLength)

        case Square.BottomLeft(_) | Square.TopRight(_) =>
          val next = turn
          next #:: loop(next, leftOnRow = rowLength + 1, rowLength = rowLength + 1)

        case s @ Square.Top(_) =>
          val next = advance(s)
          next #:: loop(next, leftOnRow = leftOnRow - 1, rowLength = rowLength)

        case s @ Square.LeftSide(_) =>
          val next = advance(s)
          next #:: loop(next, leftOnRow = leftOnRow - 1, rowLength = rowLength)

        case s @ Square.RightSide(_) =>
          val next = advance(s)
          next #:: loop(next, leftOnRow = leftOnRow - 1, rowLength = rowLength)

        case s @ Square.Bottom(_) =>
          val next = advance(s)
          next #:: loop(next, leftOnRow = leftOnRow - 1, rowLength = rowLength)
      }
    }

    Location.port #:: loop(Location.port, 1, 0)
  }

  def stressTestGrid: Stream[(Cell[Int], Memory[Int])] = {
    def loop(
      memory: Memory[Int],
      locationGen: Iterator[Location]
    ): Stream[(Cell[Int], Memory[Int])] = {
      val next = locationGen.next
      val value =
        if (next.square == Square.Port) 1
        else next.offset.neighbors.flatMap(memory.get).map(_.data).sum
      val newCell = next.toCell(value)
      val nextMemory = memory + newCell
      (newCell, nextMemory) #:: loop(nextMemory, locationGen)
    }

    loop(Memory.empty, allocator.iterator)
  }

  def stressTest(input: String): Try[String] =
    SpiralMemory.parse(input).map { target =>
      stressTestGrid
        .map(_._1)
        .dropWhile(_.data <= target)
        .headOption
        .fold("")(_.data.toString)
    }.wrapFailure(throwable => Failure(new SpiralMemoryFailure(input, throwable)))
}
