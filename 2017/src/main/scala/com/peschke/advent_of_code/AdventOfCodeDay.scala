package com.peschke.advent_of_code

import scala.util.Try

trait AdventOfCodeDay {
  type P1
  type P2

  def runPart1(input: String): Try[P1]
  def runPart2(input: String): Try[P2]

  def verifyPart1Samples(): Unit
  def verifyPart2Samples(): Unit

  lazy val name: String = {
    val raw = {
      val simpleName = getClass.getSimpleName
      simpleName.split('$').lastOption.getOrElse(simpleName)
    }
    raw.toList.sliding(2).flatMap {
      case h :: t :: Nil if h.isLower && t.isUpper => List(h, ' ')
      case h :: _ => List(h)
    }.mkString + raw.lastOption.fold("")(_.toString)
  }

  def verifySampleCases(skipPart1: Boolean = false, skipPart2: Boolean = false): Unit = {
    if (skipPart1) {
      println("Skipping part 1 sample cases")
    }
    else {
      println("Checking part 1 sample cases")
      verifyPart1Samples()
    }

    if (skipPart2) {
      println("Skipping part 2 sample cases")
    }
    else {
      println("\nChecking part 2 sample cases")
      verifyPart2Samples()
    }
  }
}

object AdventOfCodeDay {
  val all: Seq[AdventOfCodeDay] =
    Seq(
      com.peschke.advent_of_code.day1.InverseCaptcha,
      com.peschke.advent_of_code.day2.CorruptionChecksum,
      com.peschke.advent_of_code.day3.SpiralMemory,
      com.peschke.advent_of_code.day4.HighEntropyPassphrases,
      com.peschke.advent_of_code.day5.TrampolineMaze,
      com.peschke.advent_of_code.day6.MemoryReallocation,
      com.peschke.advent_of_code.day7.RecursiveCircus,
      com.peschke.advent_of_code.day8.IHeardYouLikeRegisters,
      com.peschke.advent_of_code.day9.StreamProcessing,
      com.peschke.advent_of_code.day10.KnotHash,
      com.peschke.advent_of_code.day11.HexEd,
      com.peschke.advent_of_code.day12.DigitalPlumber,
      com.peschke.advent_of_code.day13.PacketScanners,
      com.peschke.advent_of_code.day14.DiskDefragmentation,
      com.peschke.advent_of_code.day15.DuelingGenerators,
      com.peschke.advent_of_code.day16.PermutationPromenade,
      com.peschke.advent_of_code.day17.SpinLock,
      com.peschke.advent_of_code.day18.Duet
    )
}

class AdventOfCodeDayFailure(day: AdventOfCodeDay, input: String, cause: Throwable)
    extends IllegalArgumentException(s"$day failed on input:\n$input", cause)

sealed trait Result {
  def render: String
}
object Result {
  case object Pass extends Result {
    def render: String = "[Pass]"
  }

  case class Fail[A](actual: A, expected: A) extends Result {
    def render: String = s"[Fail] returned $actual, but expected $expected"
  }

  case class Abort[T <: Throwable](exception: T) extends Result {
    def render: String =
      s"""|[Fail] threw an exception:
          |$exception""".stripMargin
  }
}
