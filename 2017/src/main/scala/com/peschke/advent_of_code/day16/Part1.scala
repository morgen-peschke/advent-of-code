package com.peschke.advent_of_code
package day16

import scala.util.{Failure, Success, Try}

import cats.instances.try_._
import cats.instances.list._

import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.foldable._

import eu.timepit.refined.{refineV, refineMV}
import eu.timepit.refined.numeric._

object Part1 {
  type ErrorMsg = String
  type ErrorMsgOr[A] = Either[ErrorMsg, A]

  implicit class DanceLineOps(val dl: DanceLine) extends AnyVal {
    def locateProgram(named: Program.Name): ErrorMsgOr[Exchange.Position] =
      dl.programs.indexWhere(_.name == named) match {
        case -1 => s"Unable to find program named $named".asLeft
        case i => refineV(i)
      }

    def at(position: Exchange.Position): ErrorMsgOr[Program] =
      dl.programs
        .lift(position.value)
        .toValid(s"No program at $position")
        .toEither

    def update(position: Exchange.Position, program: Program): ErrorMsgOr[DanceLine] =
      Try(dl.programs.updated(position.value, program)) match {
        case Success(programs) => DanceLine(programs).asRight
        case Failure(ex) => s"Unable to update at $position: ${ex.getMessage}".asLeft
      }

    def swap(
      positionA: Exchange.Position,
      positionB: Exchange.Position
    ): ErrorMsgOr[DanceLine] =
      for {
        programA    <- dl.at(positionA)
        programB    <- dl.at(positionB)
        updatedOne  <- dl.update(positionB, programA)
        updatedBoth <- updatedOne.update(positionA, programB)
      } yield updatedBoth
  }

  implicit class DanceMoveOps(val dm: DanceMove) extends AnyVal {
    def applyTo(dl: DanceLine): ErrorMsgOr[DanceLine] = {
      def renderInput: String = s"${dm.render} -> ${dl.render}"

      (dm, dl) match {
        case (_, DanceLine(Vector())) => s"DanceLine is empty: ${renderInput}".asLeft
        case (Spin(0), unchanged) => unchanged.asRight
        case (Spin(c), _) if c < 0 => s"Cannot spin backwards: ${renderInput}".asLeft

        case (Spin(c), DanceLine(programs)) =>
          DanceLine((0 until c).foldLeft(programs) {
            case (init :+ tail, _) => tail +: init
          }).asRight

        case (Exchange(positionA, positionB), danceLine) =>
          danceLine.swap(positionA, positionB)

        case (Partner(nameA, nameB), danceLine) =>
          for {
            positionA <- danceLine.locateProgram(named = nameA)
            positionB <- danceLine.locateProgram(named = nameB)
            swapped   <- danceLine.swap(positionA, positionB)
          } yield swapped
      }
    }
  }

  def parse(input: String): Try[List[DanceMove]] =
    input
      .split(',')
      .toList
      .traverse { chunk =>
        Parser.danceMove.tryToParse(chunk).flatMap {
          case Right(v)  => Success(v)
          case Left(msg) => Failure(new IllegalArgumentException(msg))
        }
      }

  def dance(
    input: String,
    initial: ErrorMsgOr[DanceLine] = DanceLine.ofLength(refineMV(16))
  ): Try[ErrorMsgOr[DanceLine]] =
    parse(input).map { moves =>
      moves.foldLeft(initial) { (prev, step) =>
        prev.flatMap(step.applyTo)
      }
    }
}
