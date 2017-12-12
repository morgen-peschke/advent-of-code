package com.peschke.advent_of_code
package day8

import com.peschke.advent_of_code.AdventOfCodeDay

import cats.data.Validated.{Valid,Invalid}

import cats.syntax.validated._
import cats.syntax.either._
import cats.syntax.foldable._

import cats.instances.list._

import scala.util.Try

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
  */
object IHeardYouLikeRegisters extends AdventOfCodeDay[Amount, Nothing] {
  def runPart1(input: String): Try[Amount] = Try {
    Instruction
      .compile(input)
      .map(Part1.eval)
      .map(_.values.toList.maximumOption)
      match {
        case Valid(Some(v)) => v
        case Valid(None) =>
          throw new IHeardYouLikeRegistersFailure(
            input,
            new NoSuchElementException("Memory was empty after evaluation"))
        case Invalid(errors) =>
          val renderedErrors = errors.toList.mkString("\n")
          throw new IHeardYouLikeRegistersFailure(
            input,
            new IllegalStateException(s"Expected Valid, but was:\n$renderedErrors"))
      }
  }

  def runPart2(input: String): Try[Nothing] = ???

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
    import Operation.OpCode.{Increment, Decrement}
    import Conditional.Comparison._
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
  }

  def verifyPart2Samples(): Unit = {

  }
}
