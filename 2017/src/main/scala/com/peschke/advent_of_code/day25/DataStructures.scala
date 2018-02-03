package com.peschke.advent_of_code
package day25

import cats.{Eq, Show}

import cats.syntax.show._
import cats.syntax.foldable._

import cats.instances.int._
import cats.instances.string._
import cats.instances.vector._
import cats.instances.tuple._
import cats.instances.map._

case class Offset(raw: Int) extends AnyVal
object Offset {
  implicit val show: Show[Offset] = Show.show(_.raw.toString)
}

case class Value(raw: Int) extends AnyVal
object Value {
  implicit val show: Show[Value] = Show.show(_.raw.toString)
}

case class Tape(position: Offset, values: Map[Offset, Value])
object Tape {
  val Empty: Tape = Tape(Offset(0), Map.empty)

  def renderWindow(tape: Tape, start: Int, end: Int): String = {
    (start to end)
      .map(Offset(_))
      .map { offset =>
        val value = tape.values.get(offset).fold(0)(_.raw)
        if (offset == tape.position) s"[$value]"
        else s" $value "
      }
      .mkString("...", "", "...")
  }

  implicit val eq: Eq[Tape] = Eq.by {
    case Tape(Offset(p), values) =>
      (p, values.toVector.collect {
        case (Offset(o), Value(v)) if v != 0 => (o, v)
      })
  }
  implicit val show: Show[Tape] = Show.show { tape =>
    val positions = tape.values.keysIterator.toVector.map(_.raw)

    renderWindow(
      tape,
      positions.minimumOption.fold(-1)(_ - 1),
      positions.maximumOption.fold(1)(_ + 1))
  }
}

sealed trait Predicate

object Predicate {
  case object Is extends Predicate

  implicit val show: Show[Predicate] = Show.fromToString
}

sealed trait Direction

object Direction {
  case object Left extends Direction
  case object Right extends Direction

  implicit val show: Show[Direction] = Show.fromToString[Direction]
}

sealed trait Action

object Action {
  case class Write(value: Value) extends Action
  case class Move(offset: Offset, direction: Direction) extends Action
  case class Transition(next: State.Name) extends Action

  implicit val show: Show[Action] = Show.show {
    case Write(v) => show"write $v"
    case Move(o, d) => show"move $o slots $d"
    case Transition(n) => show"change next state to $n"
  }
}

case class Condition(predicate: Predicate, value: Value)
object Condition {
  implicit val show: Show[Condition] = Show.show {
    case Condition(predicate, value) =>
      show"$predicate $value"
  }
}

case class Rule(condition: Condition, actions: Vector[Action])

case class State(name: State.Name, rules: Vector[Rule])
object State {
  case class Name(raw: String) extends AnyVal
  object Name {
    implicit val eq: Eq[State.Name] = Eq.fromUniversalEquals
    implicit val show: Show[State.Name] = Show.show(_.raw)
  }

  implicit val eq: Eq[State] = Eq.fromUniversalEquals
  implicit val show: Show[State] = Show.show {
    case State(Name(name), rules) =>
      val renderedRules = rules.map {
        case Rule(condition, actions) =>
          (show"if $condition:" +: actions.map("     - " + _.show)).mkString("\n")
      }.mkString("\n")

    s"""--- $name ---
       |$renderedRules""".stripMargin
  }
}

case class Interval(raw: Int) extends AnyVal
object Interval {
  implicit val eq: Eq[Interval] = Eq.fromUniversalEquals
  implicit val show: Show[Interval] = Show.show(_.raw.toString)
}

case class TuringMachine(currentState: State.Name,
                         tape: Tape,
                         states: Map[State.Name, State],
                         checksumInterval: Interval)
object TuringMachine {
  implicit val eq: Eq[TuringMachine] = cats.derive.eq[TuringMachine]
  implicit val show: Show[TuringMachine] = Show.show {
    case TuringMachine(current, tape, states, checksumInterval) =>
      show"""Turing Machine about to run state $current
            |Checksum every $checksumInterval steps.
            |
            |==== Tape ===
            |$tape
            |
            |=== States ==
            |${states.values.toVector.sortBy(_.name.raw).map(_.show).mkString("\n\n")}
            |""".stripMargin
  }
}

object Parser {

  import fastparse.all._

  val EOL: P[Unit] = P("\n")

  val integer: P[Int] = P(CharIn('0' to '9').rep(min = 1).!).map(_.toInt)

  val stateName: P[State.Name] =
    P(CharIn(('a' to 'z') ++ ('A' to 'Z')).!).map(State.Name(_))

  val value: P[Value] = P(integer).map(Value(_))

  val interval: P[Interval] = P(integer).map(Interval(_))

  val predicate: P[Predicate] = P("is").map(_ => Predicate.Is)
  val condition: P[Condition] =
    P("the current value " ~ predicate ~ " " ~ value).map {
      case (p, v) => Condition(p, v)
    }

  val action: P[Action] = {
    val direction: P[Direction] = {
      val right: P[Direction] = P("right".!).map(_ => Direction.Right)
      val left: P[Direction] = P("left".!).map(_ => Direction.Left)

      right | left
    }

    val write: P[Action.Write] = P("Write the value " ~ value ~ ".").map(Action.Write)

    val move: P[Action.Move] =
      P("Move one slot to the " ~ direction ~ ".").map(Action.Move(Offset(1), _))

    val transition: P[Action.Transition] =
      P("Continue with state " ~ stateName ~ ".").map(Action.Transition)

    write | move | transition
  }

  val rule: P[Rule] = {
    val actionLine: P[Action] = " ".rep ~ "- " ~ action

    P(" ".rep ~ "If " ~ condition ~ ":" ~/ EOL ~ actionLine.rep(min = 1, sep = EOL))
      .map {
        case (c, rs) => Rule(c, rs.toVector)
      }
  }

  val state: P[State] =
    P("In state " ~ stateName ~ ":" ~/ EOL ~ rule.rep(min = 1, sep = EOL))
      .map {
        case (name, rules) => State(name, rules.toVector)
      }


  val turingMachine: P[TuringMachine] =
    P(Start ~/
      "Begin in state " ~ stateName ~ "." ~/ EOL ~
      "Perform a diagnostic checksum after " ~ interval ~ " steps." ~/ EOL ~
      EOL ~
      state.rep(min = 1, sep = EOL ~ EOL) ~ End
    ).map {
      case (initialState, checksumInterval, states) =>
        TuringMachine(
          currentState = initialState,
          tape = Tape.Empty,
          states = states.map(s => s.name -> s).toMap,
          checksumInterval)
    }
}