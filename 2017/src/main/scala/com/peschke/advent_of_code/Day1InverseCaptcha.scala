package com.peschke.advent_of_code

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * http://adventofcode.com/2017/day/1
  *
  * You're standing in a room with "digitization quarantine" written
  * in LEDs along one wall. The only door is locked, but it includes a
  * small interface. "Restricted Area - Strictly No Digitized Users
  * Allowed."
  *
  * It goes on to explain that you may only leave by solving a captcha
  * to prove you're not a human. Apparently, you only get one
  * millisecond to solve the captcha: too fast for a normal human, but
  * it feels like hours to you.
  *
  * The captcha requires you to review a sequence of digits (your
  * puzzle input) and find the sum of all digits that match the next
  * digit in the list. The list is circular, so the digit after the
  * last digit is the first digit in the list.
  *
  * For example:
  *
  * - 1122 produces a sum of 3 (1 + 2) because the first digit (1)
  *   matches the second digit and the third digit (2) matches the
  *   fourth digit.
  *
  * - 1111 produces 4 because each digit (all 1) matches the next.
  *
  * - 1234 produces 0 because no digit matches the next.
  *
  * - 91212129 produces 9 because the only digit that matches the next
  *   one is the last digit, 9.
  */
object Day1InverseCaptcha {

  class InverseCaptchaFailure(input: String, cause: Throwable)
      extends IllegalArgumentException(s"Day1InverseCaptcha.inverseCaptcha($input) failed", cause)

  def inverseCaptcha(input: String): Try[Int] =
    Try {
    val digits = input.toVector.map(_.toString.toInt)
    val rotated = {
      val (init :+ tail) = digits
      tail +: init
    }
    digits
      .zip(rotated)
      .collect {
        case (a, b) if a == b => a
      }
      .sum
  }.transform(Success(_), throwable => Failure(new InverseCaptchaFailure(input, throwable)))

  private def verifyResult(input: String, expected: Int): String = {
    val invocationStr: String = s"inverseCaptcha($input)"
    inverseCaptcha(input) match {
      case Failure(NonFatal(ex)) =>
        s"""|[Fail] $invocationStr threw an exception:
            |$ex""".stripMargin
      case Failure(ex) => throw ex
      case Success(`expected`) =>
        s"[Pass] $invocationStr == $expected"
      case Success(actual) =>
        s"[Fail] $invocationStr returned $actual, but expected $expected"
    }
  }

  def verifySampleCases(): Unit = {
    val sampleCases = Seq(
      "1122" -> 3,
      "1111" -> 4,
      "1234" -> 0,
      "91212129" -> 9
    )

    println("Checking sample cases")
    sampleCases
      .map((verifyResult _).tupled)
      .foreach(println)
  }
}
