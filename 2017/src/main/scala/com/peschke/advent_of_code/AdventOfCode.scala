package com.peschke.advent_of_code

import scopt.OptionParser

import com.peschke.advent_of_code.day1.InverseCaptcha
import com.peschke.advent_of_code.day2.CorruptionChecksum
import com.peschke.advent_of_code.day3.SpiralMemory
import com.peschke.advent_of_code.day4.HighEntropyPassphrases
import com.peschke.advent_of_code.day5.TrampolineMaze

object AdventOfCodeOpts {
  case class DayOpts(
    day: Day = Day.NoDayChosen,
    inputOpt: Option[Input] = None,
    verifySamples: Boolean = false) {

    def run(): Unit = {
      val adventOfCodeDay = day match {
        case Day.NoDayChosen =>
          throw new IllegalStateException("Should never reach this line of code")

        case Day.InverseCaptcha => InverseCaptcha
        case Day.CorruptionChecksum => CorruptionChecksum
        case Day.SpiralMemory => SpiralMemory
        case Day.HighEntropyPassphrases => HighEntropyPassphrases
        case Day.TrampolineMaze => TrampolineMaze
      }
      if (verifySamples || inputOpt.isEmpty) {
        adventOfCodeDay.verifySampleCases()
      }

      inputOpt.foreach { input =>
        val (part1, part2) = adventOfCodeDay.run(input.contents.trim)
        println(s"Part 1: ${part1.get}")
        println(s"Part 2: ${part2.get}")
      }
    }
  }

  sealed trait Day
  object Day {
    case object NoDayChosen extends Day
    case object InverseCaptcha extends Day
    case object CorruptionChecksum extends Day
    case object SpiralMemory extends Day
    case object HighEntropyPassphrases extends Day
    case object TrampolineMaze extends Day
  }

  val optParser: OptionParser[DayOpts] = new OptionParser[DayOpts]("AdventOfCode") {
    head("AdventOfCode", "2017")

    note("http://adventofcode.com/2017")

    def verifySamplesOpt =
      opt[Unit]("verify-samples")
        .text("verify the sample input, defaults to false unless no arguments are given")
        .optional
        .maxOccurs(1)
        .action((_, c) => c.copy(verifySamples = true))

    def inputArg =
      arg[Input]("<input>")
        .text("run on this input, can be '-' for stdin, a file url (file://...), or simply text")
        .optional
        .maxOccurs(1)
        .action((i, c) => c.copy(inputOpt = Some(i)))

    def inverseCaptchaCmd(cmdStr: String) = {
      cmd(cmdStr)
        .action((_, c) => c.copy(day = Day.InverseCaptcha))
        .text("  Day 1: Inverse Captcha")
        .children(note(""), verifySamplesOpt, inputArg)
      note("")
    }

    def corruptionChecksumCmd(cmdStr: String) = {
      cmd(cmdStr)
        .action((_, c) => c.copy(day = Day.CorruptionChecksum))
        .text("  Day 2: Corruption Checksum")
        .children(note(""), verifySamplesOpt, inputArg)
      note("")
    }

    def spiralMemoryCmd(cmdStr: String) = {
      cmd(cmdStr)
        .action((_, c) => c.copy(day = Day.SpiralMemory))
        .text("  Day 3: Spiral Memory")
        .children(note(""), verifySamplesOpt, inputArg)
      note("")
    }

    def highEntropyPassphrasesCmd(cmdStr: String) = {
      cmd(cmdStr)
        .action((_, c) => c.copy(day = Day.HighEntropyPassphrases))
        .text("  Day 4: High-Entropy Passphrases")
        .children(note(""), verifySamplesOpt, inputArg)
      note("")
    }

    def trampolineMazeCmd(cmdStr: String) = {
      cmd(cmdStr)
        .action((_, c) => c.copy(day = Day.TrampolineMaze))
        .text("  Day 5: A Maze of Twisty Trampolines, All Alike")
        .children(note(""), verifySamplesOpt, inputArg)
      note("")
    }

    inverseCaptchaCmd("day1")
    inverseCaptchaCmd("inverseCaptcha")

    corruptionChecksumCmd("day2")
    corruptionChecksumCmd("corruptionChecksum")

    spiralMemoryCmd("day3")
    spiralMemoryCmd("spiralMemory")

    highEntropyPassphrasesCmd("day4")
    highEntropyPassphrasesCmd("highEntropyPassphrases")

    trampolineMazeCmd("day5")
    trampolineMazeCmd("trampolineMaze")

    checkConfig {
      case DayOpts(Day.NoDayChosen, _, _) => failure("No day chosen")
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
  def processInputOrSamples[P1, P2](
    day: AdventOfCodeDay[P1, P2],
    inputOpt: Option[Input],
    verifySamples: Boolean): Unit = {
    if (verifySamples || inputOpt.isEmpty) {
      day.verifySampleCases()
    }

    inputOpt.foreach { input =>
      val (part1, part2) = day.run(input.contents.trim)
      println(s"Part 1: ${part1.get}")
      println(s"Part 2: ${part2.get}")
    }
  }

  AdventOfCodeOpts.parse(args).foreach(_.run())
}
