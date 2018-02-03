package com.peschke.advent_of_code
package day8

import cats.data.Validated.{Invalid, Valid}
import cats.instances.either._
import cats.instances.list._
import cats.instances.map._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.validated._
import cats.{Eq, Show}

import scala.util.{Failure, Success, Try}

/**
  * http://adventofcode.com/2017/day/8
  *
  * --- Day 8: I Heard You Like Registers ---
  *
  * You receive a signal directly from the CPU. Because of your recent
  * assistance with jump instructions, it would like you to compute
  * the result of a series of unusual register instructions.
  *
  * Each instruction consists of several parts: the register to
  * modify, whether to increase or decrease that register's value, the
  * amount by which to increase or decrease it, and a condition. If
  * the condition fails, skip the instruction without modifying the
  * register. The registers all start at 0. The instructions look like
  * this:
  *
  * {{{
  *    b inc 5 if a > 1
  *    a inc 1 if b < 5
  *    c dec -10 if a >= 1
  *    c inc -20 if c == 10
  * }}}
  *
  * These instructions would be processed as follows:
  *
  * - Because a starts at 0, it is not greater than 1, and so b is not
  *   modified.
  * - a is increased by 1 (to 1) because b is less than 5 (it is 0).
  * - c is decreased by -10 (to 10) because a is now greater than or
  *   equal to 1 (it is 1).
  * - c is increased by -20 (to -10) because c is equal to 10.
  * - After this process, the largest value in any register is 1.
  *
  * You might also encounter <= (less than or equal to) or != (not
  * equal to). However, the CPU doesn't have the bandwidth to tell you
  * what all the registers are named, and leaves that to you to
  * determine.
  *
  * What is the largest value in any register after completing the
  * instructions in your puzzle input?
  *
  * --- Part Two ---
  *
  * To be safe, the CPU also needs to know the highest value held in
  * any register during this process so that it can decide how much
  * memory to allocate to these operations. For example, in the above
  * instructions, the highest value ever held was 10 (in register c
  * after the third instruction was evaluated).
  */
object IHeardYouLikeRegisters extends AdventOfCodeDay {
  type P1 = Amount
  type P2 = Amount

  implicit val instructionEq: Eq[Instruction] = cats.derive.eq[Instruction]
  implicit val tokenEq: Eq[Token] = cats.derive.eq[Token]

  implicit val amountShow: Show[Amount] = cats.derive.show[Amount]
  implicit val instructionShow: Show[Instruction] = cats.derive.show[Instruction]
  implicit val registerShow: Show[Register] = cats.derive.show[Register]
  implicit val tokenShow: Show[Token] = cats.derive.show[Token]

  def compile(input: String): Try[List[Instruction]] = Instruction.compile(input) match {
    case Invalid(errors) =>
      val renderedErrors = errors.toList.mkString("\n")
      Failure(new AdventOfCodeDayFailure(
        IHeardYouLikeRegisters,
        input,
        new IllegalArgumentException(s"Input failed to compile:\n$renderedErrors")
      ))

    case Valid(Nil) =>
      Failure(new AdventOfCodeDayFailure(
        IHeardYouLikeRegisters,
        input,
        new IllegalArgumentException("Input compiled to empty list of instructions")))

    case Valid(v) => Success(v)
  }

  def optionAmountToTry(input: String)(amountOpt: Option[Amount]): Try[Amount] =
    amountOpt match {
      case Some(v) => Success(v)
      case None =>
        Failure(new AdventOfCodeDayFailure(
          IHeardYouLikeRegisters,
          input,
          new NoSuchElementException("Memory was empty after evaluation")))
    }

  def runPart1(input: String): Try[Amount] =
    compile(input)
      .map(Part1.eval)
      .map(_.values.toList.maximumOption)
      .flatMap(optionAmountToTry(input))


  def runPart2(input: String): Try[Amount] =
    compile(input)
      .map(Part2.eval _ andThen (_.iterator))
      .map { itr =>
        itr.flatMap(_.values.toList.maximumOption).toList.maximumOption
      }
      .flatMap(optionAmountToTry(input))

  private val sampleInput = """|b inc 5 if a > 1
                               |a inc 1 if b < 5
                               |c dec -10 if a >= 1
                               |c inc -20 if c == 10""".stripMargin

  private val sampleTokens = List(
    List("b", "inc", "5",   "if", "a", ">",  "1" ).map(Token(_)),
    List("a", "inc", "1",   "if", "b", "<",  "5" ).map(Token(_)),
    List("c", "dec", "-10", "if", "a", ">=", "1" ).map(Token(_)),
    List("c", "inc", "-20", "if", "c", "==", "10").map(Token(_)))

  private val sampleInstructions = {
    import Conditional.Comparison._
    import Operation.OpCode.{Decrement, Increment}
    List(
    Instruction(
      Operation(Register("b"), Increment, Amount(5)),
      Conditional(Register("a"), GreaterThan, Amount(1))),
    Instruction(
      Operation(Register("a"), Increment, Amount(1)),
      Conditional(Register("b"), LessThan, Amount(5))),
    Instruction(
      Operation(Register("c"), Decrement, Amount(-10)),
      Conditional(Register("a"), GreaterThan or EqualTo, Amount(1))),
    Instruction(
      Operation(Register("c"), Increment, Amount(-20)),
      Conditional(Register("c"), EqualTo, Amount(10))))
  }

  def verifyPart1Samples(): Unit = {
    val lineLoc = LineLocation(2)

    {
      Seq(
        ""          -> "[Line 2] Unable to tokenize: input is empty".asLeft[List[Token]],
        "         " -> "[Line 2] Unable to tokenize: input is empty".asLeft[List[Token]]
      ) ++ sampleInput.split('\n').zip(sampleTokens.map(_.asRight[Error]))
    }.map((verifyResult((Token.tokenize(_: String, lineLoc)).liftedToTry) _).tupled)
      .foreach(println)

    sampleTokens
      .zip(sampleInstructions.map(_.asRight[Error]))
      .map((verifyResult((Instruction.parse(_: List[Token], lineLoc)).liftedToTry) _).tupled)
      .foreach(println)

    println(verifyResult((Instruction.compile _).liftedToTry)(
      sampleInput,
      sampleInstructions.validNel))

    println(verifyResult((Part1.eval _).liftedToTry)(
      sampleInstructions,
      Part1.Memory(
        Register("a") -> Amount(1),
        Register("c") -> Amount(-10))))

    println(verifyResult(runPart1)(sampleInput, Amount(1)))
  }

  def verifyPart2Samples(): Unit = {
    println(verifyResult(runPart2)(sampleInput, Amount(10)))
  }
}
