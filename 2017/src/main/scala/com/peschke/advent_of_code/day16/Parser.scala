package com.peschke.advent_of_code
package day16

import cats.syntax.either._

import eu.timepit.refined.api.RefType.applyRef

object Parser {
  import fastparse.all._

  val integer: P[Int] = P(CharIn('0' to '9').rep.!.map(_.toInt))

  val position: P[Either[String,Exchange.Position]] =
    integer.map(applyRef[Exchange.Position](_))

  val programName: P[Either[String,Program.Name]] =
    P(CharIn('a' to 'p').!.map { str =>
      for {
        char <- str.asChar
        name <- applyRef[Program.Name](char)
      } yield name
    })

  val spin: P[Either[String,Spin]] = P {
    ("s" | "S") ~/ integer.map(i => Spin(i).asRight)
  }

  val exchange: P[Either[String,Exchange]] = P {
    ("x" | "X") ~/ position ~/ "/" ~/ position
  }.map {
    case (aOrError, bOrError) => for {
      a <- aOrError
      b <- bOrError
    } yield Exchange(a, b)
  }

  val partner: P[Either[String,Partner]] = P {
    ("p" | "P") ~/ programName ~/ "/" ~/ programName
  }.map {
    case (aOrError, bOrError) => for {
      a <- aOrError
      b <- bOrError
    } yield Partner(a, b)
  }

  val danceMove: P[Either[String,DanceMove]] = P(Start ~/ (spin | exchange | partner) ~/ End)
}
