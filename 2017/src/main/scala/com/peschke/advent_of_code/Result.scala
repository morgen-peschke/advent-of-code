package com.peschke.advent_of_code

import cats.{Reducible, Show}
import cats.data.NonEmptyVector
import cats.syntax.show._
import cats.instances.int._

sealed trait Result[+A]

object Result {

  case object Pass extends Result[Nothing]

  case class Fail[+A](actual: A, expected: A) extends Result[A]

  case class Abort(exception: Throwable) extends Result[Nothing]

  implicit val passShow: Show[Pass.type] = Show.show(_ => "[Pass]")

  implicit def failShow[A](implicit s: Show[A]): Show[Fail[A]] = Show.show { f =>
    val actualShow = f.actual.show
    val expectedShow = f.expected.show
    val actualShowLines = actualShow.split('\n').toVector
    val expectedShowLines = expectedShow.split('\n').toVector

    val actualHeader = "[Fail] Returned"
    val expectedHeader = "Expected"
    val maxWidthActual =
      Reducible[NonEmptyVector]
        .maximum(
          NonEmptyVector(
            actualHeader.length,
            actualShowLines.map(_.length)))
    val maxWidthExpected =
      Reducible[NonEmptyVector]
        .maximum(
          NonEmptyVector(
            expectedHeader.length,
            expectedShowLines.map(_.length)))

    val header =
      actualHeader.padTo(maxWidthActual, ' ') +
      "  |  " +
      expectedHeader.padTo(maxWidthExpected, ' ')
    val delimiter =
      ("-" * maxWidthActual) + "--+--" + ("-" * maxWidthExpected)

    val maxLen = actualShowLines.length max expectedShowLines.length

    val body =
      (actualShowLines.padTo(maxLen, "") zip expectedShowLines.padTo(maxLen, "")).map {
        case (a, e) =>
          val sep = if (a == e) "  |  "
                    else " <+> "
          a.padTo(maxWidthActual, ' ') + sep + e.padTo(maxWidthExpected, ' ')
      }

    (header +: delimiter +: body).mkString("\n")
  }

  implicit val abortShow: Show[Abort] = Show.show { a =>
    import java.io.ByteArrayOutputStream
    import java.io.PrintStream
    import java.nio.charset.StandardCharsets
    val baos = new ByteArrayOutputStream
    val ps = new PrintStream(baos, true, "utf-8")
    a.exception.printStackTrace(ps)
    val stackTrace = new String(baos.toByteArray, StandardCharsets.UTF_8)
    ps.close()
    s"""|[Fail] threw an exception:
        |$stackTrace""".stripMargin
  }

  implicit def resultShow[A](implicit s: Show[A]): Show[Result[A]] = Show.show {
    case Pass           => Pass.show
    case f @ Fail(_, _) => f.show
    case a @ Abort(_)   => a.show
  }
}
