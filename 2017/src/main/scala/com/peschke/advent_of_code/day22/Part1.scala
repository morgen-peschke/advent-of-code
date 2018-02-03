package com.peschke.advent_of_code
package day22

import scala.util.Try

object Part1 {
  implicit class FacingOps(val facing: Facing) extends AnyVal {
    def turnRight: Facing = facing match {
      case Facing.North => Facing.East
      case Facing.East  => Facing.South
      case Facing.South => Facing.West
      case Facing.West  => Facing.North
    }

    def turnLeft: Facing = facing match {
      case Facing.East  => Facing.North
      case Facing.South => Facing.East
      case Facing.West  => Facing.South
      case Facing.North => Facing.West
    }
  }

  implicit class PositionOps(val pos: Position) extends AnyVal {
    def turnRight: Position = pos.copy(facing = pos.facing.turnRight)
    def turnLeft: Position = pos.copy(facing = pos.facing.turnLeft)

    def advance: Position = pos.node match {
      case Node(row, col) =>
        pos.copy(node =
          pos.facing match {
            case Facing.North => Node(row - 1, col)
            case Facing.South => Node(row + 1, col)
            case Facing.East  => Node(row, col + 1)
            case Facing.West  => Node(row, col - 1)
          })
    }
  }

  implicit class GridOps(val grid: Grid) extends AnyVal {
    def isInfected(node: Node): Boolean = grid.infected.contains(node)

    def infect(node: Node): Grid = grid.copy(infected = grid.infected + node)
    def clean(node: Node): Grid = grid.copy(infected = grid.infected - node)
  }

  implicit class StateOps(val state: State) extends AnyVal {
    def carrierIsOnInfectedNode: Boolean = state.grid.isInfected(state.carrier.node)

    def burst: State = {
      val turnedAndFlipped =
        if (carrierIsOnInfectedNode)
          state.copy(
            carrier = state.carrier.turnRight,
            grid = state.grid.clean(state.carrier.node))
        else
          state.copy(
            carrier = state.carrier.turnLeft,
            grid = state.grid.infect(state.carrier.node))

      turnedAndFlipped.copy(carrier = turnedAndFlipped.carrier.advance)
    }
  }

  def simulate(input: String, iterations: Int): Try[SimulationResult] =
    State.parse(input).map { initialState =>
      (0 until iterations).foldLeft(SimulationResult(initialState, 0)) {
        case (SimulationResult(state, count), _) =>
          SimulationResult(state.burst, if (state.carrierIsOnInfectedNode) count else count + 1)
      }
    }
}
