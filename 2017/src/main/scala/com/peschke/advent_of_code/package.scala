package com.peschke

import scala.util.{Try, Failure, Success}
import scala.util.control.NonFatal

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
    def mapError(day: AdventOfCodeDay[_, _], input: String): Try[T] =
      t.transform(Success(_), t => Failure(new AdventOfCodeDayFailure(day, input, t)))
  }
}
