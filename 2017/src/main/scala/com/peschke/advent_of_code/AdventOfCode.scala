package com.peschke.advent_of_code

import scopt.OptionParser

import com.peschke.advent_of_code.day1.InverseCaptcha
import com.peschke.advent_of_code.day2.CorruptionChecksum
import com.peschke.advent_of_code.day3.SpiralMemory

object AdventOfCodeOpts {
  sealed trait DayOpts {
    val verifySamples: Boolean

    def as[T](implicit converter: OptsConverter[T]): T = converter.convertFrom(this)
  }

  trait OptsConverter[T] {
    def convertFrom(source: DayOpts): T
  }

  object OptsConverter {
    def apply[T](convert: DayOpts => T): OptsConverter[T] = new OptsConverter[T] {
      def convertFrom(source: DayOpts): T = convert(source)
    }
  }

  object DayOpts {
    case object NoDayChosen extends DayOpts {
      val verifySamples: Boolean = false
    }

    case class InverseCaptchaOpts(input: Option[Input], verifySamples: Boolean) extends DayOpts

    object InverseCaptchaOpts {
      implicit val convertFromOpts: OptsConverter[InverseCaptchaOpts] =
        OptsConverter[InverseCaptchaOpts] {
          case i @ InverseCaptchaOpts(_, _) => i
          case _ => InverseCaptchaOpts(None, false)
        }
    }

    case class CorruptionChecksumOpts(input: Option[Input], verifySamples: Boolean) extends DayOpts

    object CorruptionChecksumOpts {
      implicit val convertFromOpts: OptsConverter[CorruptionChecksumOpts] =
        OptsConverter[CorruptionChecksumOpts] {
          case i @ CorruptionChecksumOpts(_, _) => i
          case _ => CorruptionChecksumOpts(None, false)
        }
    }

    case class SpiralMemoryOpts(input: Option[Input], verifySamples: Boolean) extends DayOpts

    object SpiralMemoryOpts {
      implicit val convertFromOpts: OptsConverter[SpiralMemoryOpts] =
        OptsConverter[SpiralMemoryOpts] {
          case i @ SpiralMemoryOpts(_, _) => i
          case _ => SpiralMemoryOpts(None, false)
        }
    }
  }

  val optParser: OptionParser[DayOpts] = new OptionParser[DayOpts]("AdventOfCode") {
    head("AdventOfCode", "2017")

    note("http://adventofcode.com/2017")

    def verifySamplesOpt =
      opt[Unit]("verify-samples")
          .text("verify the sample input, defaults to false unless no arguments are given")
          .optional
          .maxOccurs(1)

    def inputArg =
      arg[Input]("<input>")
        .text("run on this input, can be '-' for stdin, a file url (file://...), or simply text")
        .optional
        .maxOccurs(1)

    cmd("inverseCaptcha")
      .action((_, c) => c.as[DayOpts.InverseCaptchaOpts])
      .text("  Day 1: Inverse Captcha")
      .children(
        note(""),
        verifySamplesOpt
          .action((_, c) => c.as[DayOpts.InverseCaptchaOpts].copy(verifySamples = true)),
        inputArg
          .action((i, c) => c.as[DayOpts.InverseCaptchaOpts].copy(input = Some(i)))
      )

    cmd("corruptionChecksum")
      .action((_, c) => c.as[DayOpts.CorruptionChecksumOpts])
      .text("  Day 2: Corruption Checksum")
      .children(
        note(""),
        verifySamplesOpt
          .action((_, c) => c.as[DayOpts.CorruptionChecksumOpts].copy(verifySamples = true)),
        inputArg
          .action((i, c) => c.as[DayOpts.CorruptionChecksumOpts].copy(input = Some(i)))
      )

    cmd("spiralMemory")
      .action((_, c) => c.as[DayOpts.SpiralMemoryOpts])
      .text("  Day 3: Spiral Memory")
      .children(
        note(""),
        verifySamplesOpt
          .action((_, c) => c.as[DayOpts.SpiralMemoryOpts].copy(verifySamples = true)),
        inputArg
          .action((i, c) => c.as[DayOpts.SpiralMemoryOpts].copy(input = Some(i)))
      )

    checkConfig {
      case DayOpts.NoDayChosen => failure("No day chosen")
      case _ => success
    }
  }

  def parse(args: Seq[String]): Option[DayOpts] = optParser.parse(args, DayOpts.NoDayChosen)
}

object AdventOfCode extends App {
  import AdventOfCodeOpts.DayOpts._

  def processInputOrSamples[T](
    day: AdventOfCodeDay[T],
    inputOpt: Option[Input],
    verifySamples: Boolean): Unit = {
    if (verifySamples || inputOpt.isEmpty) {
      day.verifySampleCases()
    }

    inputOpt.foreach { input =>
      day.run(input.contents.trim).zipWithIndex.foreach {
        case (result, i) => println(s"Part ${i + 1}: ${result.get}")
      }
    }
  }

  AdventOfCodeOpts
    .parse(args.map {
      case "-" => "stream://stdin"
      case x => x
    })
    .foreach {
      case NoDayChosen => throw new IllegalStateException("Should never reach this line of code")
      case InverseCaptchaOpts(inputOpt, verifySamples) =>
        processInputOrSamples(InverseCaptcha, inputOpt, verifySamples)

      case CorruptionChecksumOpts(inputOpt, verifySamples) => {
        processInputOrSamples(CorruptionChecksum, inputOpt, verifySamples)
      }

      case SpiralMemoryOpts(inputOpt, verifySamples) => {
        processInputOrSamples(SpiralMemory, inputOpt, verifySamples)
      }
    }
}
