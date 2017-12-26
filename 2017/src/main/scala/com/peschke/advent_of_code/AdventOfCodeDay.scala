package com.peschke.advent_of_code

import scala.util.Try

trait AdventOfCodeDay[P1, P2] {
  def verifySampleCases(): Unit = {
    println("Checking part 1 sample cases")
    verifyPart1Samples()

    println("\nChecking part 2 sample cases")
    verifyPart2Samples()
  }

  def runPart1(input: String): Try[P1]
  def runPart2(input: String): Try[P2]

  def verifyPart1Samples(): Unit
  def verifyPart2Samples(): Unit
}

class AdventOfCodeDayFailure(day: AdventOfCodeDay[_, _], input: String, cause: Throwable)
    extends IllegalArgumentException(s"$day failed on input:\n$input", cause)
