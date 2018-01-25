package com.peschke.advent_of_code
package day22

import cats.{Eq, Show}
import cats.syntax.eq._
import cats.syntax.foldable._
import cats.syntax.show._
import cats.instances.int._
import cats.instances.vector._

import scala.util.Try

case class Node(row: Int, col: Int)

object Node {
  implicit val show: Show[Node] = Show.show {
    case Node(row, col) => s"($row,$col)"
  }
}

sealed trait NodeState extends Product with Serializable
object NodeState {
  case object Clean extends NodeState
  case object Weakened extends NodeState
  case object Infected extends NodeState
  case object Flagged extends NodeState
}

case class Grid(infected: Set[Node] = Set.empty,
                weakened: Set[Node] = Set.empty,
                flagged: Set[Node] = Set.empty)

object Grid {
  def anchor(grid: Grid): Grid = {
    val rowOffset = grid.infected.map(_.row).min
    val colOffset = grid.infected.map(_.col).min
    Grid(grid.infected.map {
      case Node(row, col) => Node(row - rowOffset, col - colOffset)
    })
  }

  implicit val gridEq: Eq[Grid] = Eq.instance[Grid] { (x, y) =>
    anchor(x).infected == anchor(y).infected
  }

  implicit val gridShow: Show[Grid] = Show.show { grid =>
    val maxRowOffset =
        Vector(
          grid.infected.toVector.map(_.row.abs).maximumOption,
          grid.weakened.toVector.map(_.row.abs).maximumOption,
          grid.flagged.toVector.map(_.row.abs).maximumOption
        ).flatten.maximumOption.getOrElse(0)
    val maxColOffset =
        Vector(
          grid.infected.toVector.map(_.col.abs).maximumOption,
          grid.weakened.toVector.map(_.col.abs).maximumOption,
          grid.flagged.toVector.map(_.col.abs).maximumOption
        ).flatten.maximumOption.getOrElse(0)

    (-maxRowOffset to maxRowOffset)
      .map { rowIndex =>
        (-maxColOffset to maxColOffset)
          .map { colIndex =>
            val node = Node(rowIndex, colIndex)
            if (grid.infected.contains(node)) '#'
            else if (grid.flagged.contains(node)) 'F'
            else if (grid.weakened.contains(node)) 'W'
            else '.'
          }
          .mkString
      }
      .mkString("\n")
  }

  def apply(nodes: Node*): Grid = Grid(nodes.toSet)

  def parseOrDie(input: String): Grid = parse(input).get

  def parse(input: String): Try[Grid] = Try {
    val chars = input.trim.split('\n').map(_.toVector).toVector
    val length = chars.map(_.length).distinct match {
      case Vector(l) => l
      case _ => throw new IllegalArgumentException("Input should be rectangular")
    }
    val rowOffset = (length - 1) / 2
    val colOffset = (chars.length - 1) / 2
    val nodesAndStates = chars.zipWithIndex.flatMap {
      case (row, rowIndex) =>
        val nodes =
          row
            .indices
            .map(colIndex => Node(rowIndex - rowOffset, colIndex - colOffset))

        val nodeStates =
          row.map {
            case '#' => NodeState.Infected
            case 'F' => NodeState.Flagged
            case 'W' => NodeState.Weakened
            case '.' => NodeState.Clean
            case c => throw new IllegalArgumentException(s"Unexpected character in map '$c'")
          }

        nodeStates zip nodes
    }
    Grid(
      infected = nodesAndStates.collect { case (NodeState.Infected, n) => n }.toSet,
      weakened = nodesAndStates.collect { case (NodeState.Weakened, n) => n }.toSet,
      flagged = nodesAndStates.collect { case (NodeState.Flagged, n) => n }.toSet
    )
  }

}

sealed trait Facing
object Facing {
  case object North extends Facing
  case object South extends Facing
  case object East  extends Facing
  case object West  extends Facing

  implicit val show: Show[Facing] = Show.fromToString[Facing]
}

case class Position(facing: Facing, node: Node)

case class State(grid: Grid, carrier: Position)

object State {
  implicit val eq: Eq[State] = Eq.instance[State] { (x, y) =>
    (x.carrier == y.carrier) && x.grid === y.grid
  }

  implicit val show: Show[State] = Show.show {
    case State(grid, Position(facing, node)) =>
    show"""Carrier is facing $facing at $node
          |$grid
          |""".stripMargin
  }

  def parse(input: String): Try[State] =
    Grid.parse(input).map(grid => State(grid, Position(Facing.North, Node(0, 0))))
}

case class SimulationResult(state: State, burstsCausingInfections: Int)
object SimulationResult {
  import cats.instances.int._
  implicit val eq: Eq[SimulationResult] = cats.derive.eq[SimulationResult]
  implicit val show: Show[SimulationResult] = {
    case SimulationResult(state, burstCount) =>
      s"""$burstCount bursts caused infections
         |${state.show}""".stripMargin
  }
}