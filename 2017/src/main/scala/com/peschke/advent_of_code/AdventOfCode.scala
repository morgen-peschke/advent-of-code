package com.peschke.advent_of_code

import scopt.OptionParser

object AdventOfCodeOpts {
  case class DayOpts(
    day: Option[AdventOfCodeDay] = None,
    inputOpt: Option[Input] = None,
    verifySamples: Boolean = false,
    skipPart1: Boolean = false,
    skipPart2: Boolean = false
  ) {

    def withDay(d: AdventOfCodeDay): DayOpts = copy(day = Some(d))

    def run(): Unit = {
      val adventOfCodeDay = day.getOrElse {
        throw new IllegalStateException("Should never reach this line of code")
      }
      if (verifySamples || inputOpt.isEmpty) {
        adventOfCodeDay.verifySampleCases(skipPart1, skipPart2)
        inputOpt.foreach(_ => println())
      }
      inputOpt.foreach { input =>
        val cached = input.contents
        if (skipPart1) {
          println("Part 1: [Skipped]")
        }
        else {
          println(s"Part 1: ${adventOfCodeDay.runPart1(cached).get}")
        }

        if (skipPart2) {
          println("Part 2: [Skipped]")
        }
        else {
          println(s"Part 2: ${adventOfCodeDay.runPart2(cached).get}")
        }
      }
    }
  }

  val optParser: OptionParser[DayOpts] = new OptionParser[DayOpts]("AdventOfCode") {
    head("AdventOfCode", "2017")

    note("http://adventofcode.com/2017\n")

    def verifyOpt =
      opt[Unit]("verify")
        .text("verify the sample input, defaults to false unless no arguments are given")
        .optional
        .action((_, c) => c.copy(verifySamples = true))

    def skipOpt =
      opt[Int]("skip")
        .valueName("1|2")
        .text("Skip either part 1 or part 2")
        .optional
        .action((v, c) => v match {
          case 1 => c.copy(skipPart1 = true)
          case 2 => c.copy(skipPart2 = true)
          case _ => throw new IllegalArgumentException(s"Expected '1' or '2'")
        })

    def inputArg =
      arg[Input]("input")
        .text {
          "run on this input, " +
            "can be '-' for stdin, " +
            "a file url (file://...), or simply text"
        }
        .optional
        .maxOccurs(1)
        .action((i, c) => c.copy(inputOpt = Some(i)))

    def dayOpts[T](showSummary: Boolean) =
      if (showSummary)
        Seq(
          verifyOpt.hidden(),
          skipOpt.hidden(),
          inputArg.hidden()
        )
      else
        Seq(
          note(""),
          note("These options are standard to all day* commands:"),
          verifyOpt,
          skipOpt,
          inputArg
        )

    AdventOfCodeDay.all.zipWithIndex.foreach {
      case (day, zeroBasedIndex) =>
        val index = zeroBasedIndex + 1

        cmd(s"day$index")
          .action((_, c) => c.withDay(day))
          .text(s"  Day $index: ${day.name}")
          .children(dayOpts(zeroBasedIndex != 0):_ *)
        note("")
    }

    help("help")

    checkConfig {
      case DayOpts(None, _, _, _, _) => failure("No day chosen")
      case DayOpts(Some(day), _, _, true, true) =>
        failure(s"Cannot skip both parts 1 & 2 of ${day.name}")
      case _ => success
    }
  }

  def parse(args: Seq[String]): Option[DayOpts] =
    optParser.parse(
      args.map {
        case "-" => "stream://stdin"
        case x => x
      },
      DayOpts())
}

object AdventOfCode extends App {
  AdventOfCodeOpts.parse(args).foreach(_.run())
}
