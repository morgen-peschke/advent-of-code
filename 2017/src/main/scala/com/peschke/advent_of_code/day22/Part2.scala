package com.peschke.advent_of_code.day22

import scala.util.Try

object Part2 {
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

    def reverse: Facing = facing match {
      case Facing.North => Facing.South
      case Facing.South => Facing.North
      case Facing.East  => Facing.West
      case Facing.West  => Facing.East
    }
  }

  implicit class PositionOps(val pos: Position) extends AnyVal {
    def turnRight: Position = pos.copy(facing = pos.facing.turnRight)
    def turnLeft: Position = pos.copy(facing = pos.facing.turnLeft)
    def reverse: Position = pos.copy(facing = pos.facing.reverse)

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
    def nodeState(node: Node): NodeState =
      if (grid.infected.contains(node)) NodeState.Infected
      else if (grid.weakened.contains(node)) NodeState.Weakened
      else if (grid.flagged.contains(node)) NodeState.Flagged
      else NodeState.Clean

    def without(node: Node): Grid = grid.copy(
      infected = grid.infected - node,
      weakened = grid.weakened - node,
      flagged = grid.flagged - node)

    def weaken(node: Node): Grid = without(node).copy(weakened = grid.weakened + node)
    def infect(node: Node): Grid = without(node).copy(infected = grid.infected + node)
    def flag(node: Node): Grid = without(node).copy(flagged = grid.flagged + node)
    def clean(node: Node): Grid = without(node)
  }

  implicit class StateOps(val state: State) extends AnyVal {
    def carrierNodeState: NodeState = state.grid.nodeState(state.carrier.node)

    def burst: State = {
      val turnedAndModified = carrierNodeState match {
        case NodeState.Clean =>
          state.copy(
            carrier = state.carrier.turnLeft,
            grid = state.grid.weaken(state.carrier.node))
        case NodeState.Weakened =>
          state.copy(grid = state.grid.infect(state.carrier.node))
        case NodeState.Infected =>
          state.copy(
            carrier = state.carrier.turnRight,
            grid = state.grid.flag(state.carrier.node))
        case NodeState.Flagged =>
          state.copy(
            carrier = state.carrier.reverse,
            grid = state.grid.clean(state.carrier.node))
      }

      turnedAndModified.copy(carrier = turnedAndModified.carrier.advance)
    }
  }

  def simulate(input: String, iterations: Int): Try[SimulationResult] =
    State.parse(input).map { initialState =>
      (0 until iterations).foldLeft(SimulationResult(initialState, 0)) {
        case (SimulationResult(state, count), _) =>
          SimulationResult(
            state.burst,
            if (state.carrierNodeState == NodeState.Weakened) count + 1
            else count)
      }
    }
}
