package com.peschke

import scala.util.{Try, Failure, Success}
import fastparse.all.P
import fastparse.core.Parsed

import cats.{Eq, Show}
import cats.syntax.eq._
import cats.syntax.show._
import cats.syntax.either._

package object advent_of_code {
  implicit class EitherOps[A,B](val e: Either[A,B]) extends AnyVal {
    def asRightResult(expected: B)(implicit eq: Eq[B]): Result[Either[A,B]] =
      e match {
        case Right(actual) if actual === expected => Result.Pass
        case _                                    => Result.Fail(e, expected.asRight)
      }

    def asLeftResult(expected: A)(implicit eq: Eq[A]): Result[Either[A,B]] =
      e match {
        case Left(actual) if actual === expected => Result.Pass
        case _                                   => Result.Fail(e, expected.asLeft)
      }
  }

  implicit class TryOps[A](val t: Try[A]) extends AnyVal {
    def mapError(day: AdventOfCodeDay, input: String): Try[A] =
      t.transform(Success(_), t => Failure(new AdventOfCodeDayFailure(day, input, t)))

    def asResult(expected: A)(implicit eq: Eq[A]): Result[A] = t match {
      case Success(actual) if actual === expected => Result.Pass
      case Success(actual)     => Result.Fail(actual, expected)
      case Failure(ex)         => Result.Abort(ex)
    }
  }

  implicit class FunctionTryOps[I,O](val f: I => O) extends AnyVal {
    def liftedToTry: I => Try[O] = (i: I) => Try(f(i))
  }

  def verifyResult[I, T](f: I => Try[T])
                        (input: I, expected: T)
                        (implicit eq: Eq[T], show: Show[Result[T]]): String =
    f(input).asResult(expected).show

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
