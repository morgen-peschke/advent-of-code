package com.peschke.advent_of_code
package day25

import scala.util.Try

object Part1 {

  implicit class TapeOps(val tape: Tape) extends AnyVal {
    def value: Value = tape.values.getOrElse(tape.position, Value(0))

    def write(v: Value): Tape = tape.copy(values = tape.values.updated(tape.position, v))

    def move(d: Direction, o: Offset): Tape =
      tape.copy(
        position = Offset(tape.position.raw + (d match {
          case Direction.Left  => -o.raw
          case Direction.Right => o.raw
        })))
  }

  implicit class PredicateOps(val predicate: Predicate) extends AnyVal {
    def check(a: Value, b: Value): Boolean = predicate match {
      case Predicate.Is => a == b
    }
  }

  implicit class ConditionOps(val condition: Condition) extends AnyVal {
    def check(value: Value): Boolean = condition.predicate.check(value, condition.value)
  }

  implicit class RuleOps(val rule: Rule) extends AnyVal {
    def isApplicable(tape: Tape): Boolean = rule.condition.check(tape.value)
  }

  implicit class TuringMachineOps(val tm: TuringMachine) extends AnyVal {
    def step: TuringMachine = {
      val activeState =
        tm.states.getOrElse(
          tm.currentState,
          throw new IllegalStateException(s"TuringMachine in state in unknown state: ${tm.currentState}"))

      activeState.rules.filter(_.isApplicable(tm.tape)) match {
        case Vector()     => throw new IllegalStateException(
          s"No rules valid for current state <${tm.currentState}> and tape value <${tm.tape.value}>")
        case _ +: _ +: _  => throw new IllegalStateException(
          s"Multiple rules valid for current state <${tm.currentState}> and tape value <${tm.tape.value}>" +
          activeState.rules.mkString("\n", "\n", ""))
        case Vector(rule) =>
          rule.actions.foldLeft(tm)(_ execute _)
      }
    }

    def execute(a: Action): TuringMachine = a match {
      case Action.Write(v)   => tm.copy(tape = tm.tape.write(v))
      case Action.Move(o, d) => tm.copy(tape = tm.tape.move(d, o))
      case Action.Transition(sn) => tm.copy(currentState = sn)
    }
  }

  implicit class IteratorOps[T](val i: Iterator[T]) extends AnyVal {
    def last: T = {
      def loop(prev: T): T =
        if (i.hasNext) loop(i.next())
        else prev
      loop(i.next())
    }
  }

  def run(initial: TuringMachine): Try[Iterator[(Int, TuringMachine)]] = Try {
    Iterator
      .iterate(0 -> initial) {
        case (index, prev) => (index + 1, prev.step)
      }
      .takeWhile(_._1 <= initial.checksumInterval.raw)
  }

  def getChecksum(input: String): Try[Int] =
    Parser
      .turingMachine
      .tryToParse(input)
      .flatMap(run)
      .map(_.last._2.tape.values.values.map(_.raw).sum)
      .mapError(TheHaltingProblem, input)
}
