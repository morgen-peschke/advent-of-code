package com.peschke.advent_of_code

import scala.util.{Try, Failure, Success}
import scala.util.control.NonFatal

trait AdventOfCodeDay[T] {
  def run(input: String): Seq[Try[T]]
  def verifySampleCases(): Unit
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
