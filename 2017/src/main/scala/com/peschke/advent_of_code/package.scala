package com.peschke

import scala.util.{Try, Failure, Success}
import scala.util.control.NonFatal
import fastparse.all.P
import fastparse.core.Parsed

package object advent_of_code {
  implicit class FunctionTryOps[I,O](val f: I => O) extends AnyVal {
    def liftedToTry: I => Try[O] = (i: I) => Try(f(i))
  }

  def verifyResult[I, T](f: I => Try[T])(input: I, expected: T): String = {
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
    def mapError(day: AdventOfCodeDay, input: String): Try[T] =
      t.transform(Success(_), t => Failure(new AdventOfCodeDayFailure(day, input, t)))
  }

  implicit class ParserOps[T](val parser: P[T]) extends AnyVal {
    def tryToParse(input: String): Try[T] =
      parser.parse(input) match {
        case Parsed.Success(value, _) => Success(value)
        case f @ Parsed.Failure(_, _, _) =>
          Failure(new IllegalArgumentException(s"""|Parsing failure: ${f.msg}
                                                   |---- Input ----
                                                   |$input""".stripMargin))
      }
  }
}
