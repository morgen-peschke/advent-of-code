package com.peschke.advent_of_code

import cats.Show
import cats.syntax.show._

sealed trait Result[+A]
object Result {
  case object Pass extends Result[Nothing]
  case class Fail[+A](actual: A, expected: A) extends Result[A]
  case class Abort(exception: Throwable) extends Result[Nothing]

  implicit val passShow: Show[Pass.type] = Show.show(_ => "[Pass]")

  implicit def failShow[A](implicit s: Show[A]): Show[Fail[A]] = Show.show { f =>
    val actualShow = f.actual.show
    val expectedShow = f.expected.show
    if (actualShow.contains("\n") || expectedShow.contains("\n"))
      s"""[Fail] Returned:
         |$actualShow
         |Expected:
         |$expectedShow
       """.stripMargin
    else s"[Fail] returned $actualShow, but expected $expectedShow"
  }

  implicit val abortShow: Show[Abort] = Show.show { a =>
    s"""|[Fail] threw an exception:
        |${a.exception}""".stripMargin
  }

  implicit def resultShow[A](implicit s: Show[A]): Show[Result[A]] = Show.show {
    case Pass           => Pass.show
    case f @ Fail(_, _) => f.show
    case a @ Abort(_)   => a.show
  }
}
