package com.peschke.advent_of_code
package day1

import scala.util.{Failure, Try}

import com.peschke.advent_of_code.AdventOfCodeDay

/**
  * http://adventofcode.com/2017/day/1
  * --- Day 1: Inverse Captcha ---
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
  *
  * --- Part Two ---
  *
  * You notice a progress bar that jumps to 50%
  * completion. Apparently, the door isn't yet satisfied, but it did
  * emit a star as encouragement. The instructions change:
  *
  * Now, instead of considering the next digit, it wants you to
  * consider the digit halfway around the circular list. That is, if
  * your list contains 10 items, only include a digit in your sum if
  * the digit 10/2 = 5 steps forward matches it. Fortunately, your
  * list has an even number of elements.
  *
  * For example:
  *
  * - 1212 produces 6: the list contains 4 items, and all four digits
  *   match the digit 2 items ahead.
  * - 1221 produces 0, because every comparison is between a 1 and a
  *   2.
  * - 123425 produces 4, because both 2s match each other, but no
  *   other digit has a match.
  * - 123123 produces 12.
  * - 12131415 produces 4.
  */
object InverseCaptcha extends AdventOfCodeDay[Int, Int]{

  class InverseCaptchaFailure(input: String, cause: Throwable)
      extends IllegalArgumentException(s"InverseCaptcha failed on input:\n$input", cause)

  def runPart1(input: String): Try[Int] = inverseCaptchaPart1(input)
  def runPart2(input: String): Try[Int] = inverseCaptchaPart2(input)

  def parse(input: String): Try[Vector[Int]] = Try {
    input.trim.toVector.map(_.toString.toInt)
  }

  def inverseCaptchaPart1(input: String): Try[Int] =
    parse(input).map { digits =>
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
    }.wrapFailure(throwable => Failure(new InverseCaptchaFailure(input, throwable)))

  def inverseCaptchaPart2(input: String): Try[Int] =
    parse(input).map { digits =>
      digits
        .zip {
          val (pre, post) = digits.splitAt(digits.length / 2)
          post ++ pre
        }
        .collect {
          case (a, b) if a == b => a
        }
        .sum
    }.wrapFailure(throwable => Failure(new InverseCaptchaFailure(input, throwable)))

  def verifyPart1Samples(): Unit = {
    Seq(
      "1122" -> 3,
      "1111" -> 4,
      "1234" -> 0,
      "91212129" -> 9
    ).map((verifyResult(inverseCaptchaPart1 _) _).tupled).foreach(println)
  }

  def verifyPart2Samples(): Unit = {
    Seq(
      "1212" -> 6,
      "1221" -> 0,
      "123425" -> 4,
      "123123" -> 12,
      "12131415" -> 4
    ).map((verifyResult(inverseCaptchaPart2) _).tupled).foreach(println)
  }
}
