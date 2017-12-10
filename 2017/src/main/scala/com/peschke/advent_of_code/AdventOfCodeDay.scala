package com.peschke.advent_of_code

import scala.util.{Try, Failure, Success}
import scala.util.control.NonFatal

trait AdventOfCodeDay[P1, P2] {
  def run(input: String): (Try[P1], Try[P2]) = (runPart1(input), runPart2(input))
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

object AdventOfCodeDay {
  def verifyResult[T](f: String => Try[T])(input: String, expected: T): String = {
    f(input) match {
      case Success(`expected`) => s"[Pass]"
      case Success(actual) => s"[Fail] returned $actual, but expected $expected"
      case Failure(NonFatal(ex)) =>
        s"""|[Fail] threw an exception:
            |$ex""".stripMargin
      case Failure(ex) => throw ex
    }
  }

  implicit class TryOpts[T](val t: Try[T]) extends AnyVal {
    def wrapFailure(f: Throwable => Failure[T]): Try[T] = t.transform(Success(_), f)
  }
}
