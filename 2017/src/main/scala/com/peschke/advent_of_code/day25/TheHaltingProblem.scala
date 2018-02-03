package com.peschke.advent_of_code
package day25

import cats.syntax.show._
import cats.instances.int._
import cats.instances.tuple._
import cats.instances.vector._
import com.peschke.advent_of_code.day25.Predicate.Is
import monocle.macros.GenLens

import scala.util.Try

/**
  * http://adventofcode.com/2017/day/25
  *
  * --- Day 25: The Halting Problem ---
  *
  * Following the twisty passageways deeper and deeper into the CPU,
  * you finally reach the core of the computer. Here, in the expansive
  * central chamber, you find a grand apparatus that fills the entire
  * room, suspended nanometers above your head.
  *
  * You had always imagined CPUs to be noisy, chaotic places, bustling
  * with activity. Instead, the room is quiet, motionless, and dark.
  *
  * Suddenly, you and the CPU's garbage collector startle each
  * other. "It's not often we get many visitors here!", he says. You
  * inquire about the stopped machinery.
  *
  * "It stopped milliseconds ago; not sure why. I'm a garbage
  * collector, not a doctor." You ask what the machine is for.
  *
  * "Programs these days, don't know their origins. That's the Turing
  * machine! It's what makes the whole computer work." You try to
  * explain that Turing machines are merely models of computation, but
  * he cuts you off. "No, see, that's just what they want you to
  * think. Ultimately, inside every CPU, there's a Turing machine
  * driving the whole thing! Too bad this one's broken. We're doomed!"
  *
  * You ask how you can help. "Well, unfortunately, the only way to
  * get the computer running again would be to create a whole new
  * Turing machine from scratch, but there's no way you can-" He
  * notices the look on your face, gives you a curious glance, shrugs,
  * and goes back to sweeping the floor.
  *
  * You find the Turing machine blueprints (your puzzle input) on a
  * tablet in a nearby pile of debris. Looking back up at the broken
  * Turing machine above, you can start to identify its parts:
  *
  *  - A tape which contains 0 repeated infinitely to the left and
  *    right.
  *  - A cursor, which can move left or right along the tape and read
  *    or write values at its current position.
  *  - A set of states, each containing rules about what to do based
  *    on the current value under the cursor.
  *  - Each slot on the tape has two possible values: 0 (the starting
  *    value for all slots) and 1. Based on whether the cursor is
  *    pointing at a 0 or a 1, the current state says what value to
  *    write at the current position of the cursor, whether to move
  *    the cursor left or right one slot, and which state to use next.
  *
  * For example, suppose you found the following blueprint:
  *
  * {{{
  * Begin in state A.
  * Perform a diagnostic checksum after 6 steps.
  *
  * In state A:
  *   If the current value is 0:
  *     - Write the value 1.
  *     - Move one slot to the right.
  *     - Continue with state B.
  *   If the current value is 1:
  *     - Write the value 0.
  *     - Move one slot to the left.
  *     - Continue with state B.
  *
  * In state B:
  *   If the current value is 0:
  *     - Write the value 1.
  *     - Move one slot to the left.
  *     - Continue with state A.
  *   If the current value is 1:
  *     - Write the value 1.
  *     - Move one slot to the right.
  *     - Continue with state A.
  * }}}
  *
  * Running it until the number of steps required to take the listed
  * diagnostic checksum would result in the following tape
  * configurations (with the cursor marked in square brackets):
  *
  * {{{
  * ... 0  0  0 [0] 0  0 ... (before any steps; about to run state A)
  * ... 0  0  0  1 [0] 0 ... (after 1 step;     about to run state B)
  * ... 0  0  0 [1] 1  0 ... (after 2 steps;    about to run state A)
  * ... 0  0 [0] 0  1  0 ... (after 3 steps;    about to run state B)
  * ... 0 [0] 1  0  1  0 ... (after 4 steps;    about to run state A)
  * ... 0  1 [1] 0  1  0 ... (after 5 steps;    about to run state B)
  * ... 0  1  1 [0] 1  0 ... (after 6 steps;    about to run state A)
  * }}}
  *
  * The CPU can confirm that the Turing machine is working by taking a
  * diagnostic checksum after a specific number of steps (given in the
  * blueprint). Once the specified number of steps have been executed,
  * the Turing machine should pause; once it does, count the number of
  * times 1 appears on the tape. In the above example, the diagnostic
  * checksum is 3.
  *
  * Recreate the Turing machine and save the computer! What is the
  * diagnostic checksum it produces once it's working again?
  */
object TheHaltingProblem extends AdventOfCodeDay {
  override type P1 = Int
  override type P2 = String

  override def runPart1(input: String): Try[P1] = Part1.getChecksum(input.trim)

  override def runPart2(input: String): Try[P2] = Try("There is no part 2")

  private val sampleInput =
    """Begin in state A.
      |Perform a diagnostic checksum after 6 steps.
      |
      |In state A:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state B.
      |  If the current value is 1:
      |    - Write the value 0.
      |    - Move one slot to the left.
      |    - Continue with state B.
      |
      |In state B:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the left.
      |    - Continue with state A.
      |  If the current value is 1:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state A.""".stripMargin

  override def verifyPart1Samples(): Unit = {
    val sampleMachine = TuringMachine(
      currentState = State.Name("A"),
      tape = Tape.Empty,
      states = Map(
        State.Name("A") -> State(
          name = State.Name("A"),
          rules = Vector(
            Rule(Condition(Is, Value(0)), Vector(
              Action.Write(Value(1)),
              Action.Move(Offset(1), Direction.Right),
              Action.Transition(State.Name("B")))),
            Rule(Condition(Is, Value(1)), Vector(
              Action.Write(Value(0)),
              Action.Move(Offset(1), Direction.Left),
              Action.Transition(State.Name("B")))))),
        State.Name("B") -> State(
          name = State.Name("B"),
          rules = Vector(
            Rule(Condition(Is, Value(0)), Vector(
              Action.Write(Value(1)),
              Action.Move(Offset(1), Direction.Left),
              Action.Transition(State.Name("A")))),
            Rule(Condition(Is, Value(1)), Vector(
              Action.Write(Value(1)),
              Action.Move(Offset(1), Direction.Right),
              Action.Transition(State.Name("A"))))))
      ),
      checksumInterval = Interval(6))

    println("--- parsing ---")
    sampleMachine.states.get(State.Name("A")).foreach { stateA =>
      println(
        Parser
          .state
          .tryToParse(sampleInput.split('\n').drop(3).mkString("\n"))
          .asResult(stateA)
          .show)
    }
    println(
      Parser
        .turingMachine
        .tryToParse(sampleInput)
        .asResult(sampleMachine)
        .show)

//    implicit val runResultStepShow: Show[(Int, TuringMachine)] = Show.show {
//      case (stepNumber, tm) =>
//        s"${Tape.renderWindow(tm.tape, -3, 2)} (after step $stepNumber; about to run state ${tm.currentState.raw}"
//    }
//    implicit val unwrapRunResults: Show[Vector[(Int, TuringMachine)]] = unwrappedVectorShow(runResultStepShow)

    def state(s: State.Name) = GenLens[TuringMachine](_.currentState).set(s)
    def position(o: Offset) = GenLens[TuringMachine](_.tape.position).set(o)
    def values(vs: (Offset,Value)*) =
      GenLens[TuringMachine](_.tape.values).modify(_ ++ vs)

    def tape(o: Offset, vs: (Offset,Value)*) =
      position(o) compose values(vs:_*)

    println("--- execution ---")
    println(
      Part1
        .run(sampleMachine)
        .map(_.toVector)
        .asResult(Vector[TuringMachine => TuringMachine](
          identity,
          state(State.Name("B")) compose tape(
            Offset(1),
            Offset(0) -> Value(1)
          ),
          tape(
            Offset(0),
            Offset(0) -> Value(1),
            Offset(1) -> Value(1)
          ),
          state(State.Name("B")) compose tape(
            Offset(-1),
            Offset(1) -> Value(1)
          ),
          tape(
            Offset(-2),
            Offset(1) -> Value(1),
            Offset(-1) -> Value(1)
          ),
          state(State.Name("B")) compose tape(
            Offset(-1),
            Offset(1) -> Value(1),
            Offset(-1) -> Value(1),
            Offset(-2) -> Value(1)
          ),
          tape(
            Offset(0),
            Offset(1) -> Value(1),
            Offset(-1) -> Value(1),
            Offset(-2) -> Value(1)
          )
        ).map(_.apply(sampleMachine)).zipWithIndex.map(_.swap)).show)
    println(Part1.getChecksum(sampleInput).asResult(3).show)
  }

  override def verifyPart2Samples(): Unit = ()
}
