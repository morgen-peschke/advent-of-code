package com.peschke.advent_of_code

import scopt.OptionParser

import com.peschke.advent_of_code.day1.InverseCaptcha
import com.peschke.advent_of_code.day2.CorruptionChecksum
import com.peschke.advent_of_code.day3.SpiralMemory
import com.peschke.advent_of_code.day4.HighEntropyPassphrases
import com.peschke.advent_of_code.day5.TrampolineMaze
import com.peschke.advent_of_code.day6.MemoryReallocation
import com.peschke.advent_of_code.day7.RecursiveCircus
import com.peschke.advent_of_code.day8.IHeardYouLikeRegisters
import com.peschke.advent_of_code.day9.StreamProcessing
import com.peschke.advent_of_code.day10.KnotHash
import com.peschke.advent_of_code.day11.HexEd
import com.peschke.advent_of_code.day12.DigitalPlumber
import com.peschke.advent_of_code.day13.PacketScanners

object AdventOfCodeOpts {
  case class DayOpts(
    day: Day = Day.NoDayChosen,
    inputOpt: Option[Input] = None,
    verifySamples: Boolean = false) {

    def run(): Unit = {
      val adventOfCodeDay = day match {
        case Day.NoDayChosen =>
          throw new IllegalStateException("Should never reach this line of code")

        case Day.InverseCaptcha         => InverseCaptcha
        case Day.CorruptionChecksum     => CorruptionChecksum
        case Day.SpiralMemory           => SpiralMemory
        case Day.HighEntropyPassphrases => HighEntropyPassphrases
        case Day.TrampolineMaze         => TrampolineMaze
        case Day.MemoryReallocation     => MemoryReallocation
        case Day.RecursiveCircus        => RecursiveCircus
        case Day.IHeardYouLikeRegisters => IHeardYouLikeRegisters
        case Day.StreamProcessing       => StreamProcessing
        case Day.KnotHash               => KnotHash
        case Day.HexEd                  => HexEd
        case Day.DigitalPlumber         => DigitalPlumber
        case Day.PacketScanners         => PacketScanners
      }
      if (verifySamples || inputOpt.isEmpty) {
        adventOfCodeDay.verifySampleCases()
        inputOpt.foreach(_ => println())
      }
      inputOpt.foreach { input =>
        val trimmed = input.contents.trim
        println(s"Part 1: ${adventOfCodeDay.runPart1(trimmed).get}")
        println(s"Part 2: ${adventOfCodeDay.runPart2(trimmed).get}")
      }
    }
  }

  sealed trait Day
  object Day {
    case object NoDayChosen            extends Day
    case object InverseCaptcha         extends Day
    case object CorruptionChecksum     extends Day
    case object SpiralMemory           extends Day
    case object HighEntropyPassphrases extends Day
    case object TrampolineMaze         extends Day
    case object MemoryReallocation     extends Day
    case object RecursiveCircus        extends Day
    case object IHeardYouLikeRegisters extends Day
    case object StreamProcessing       extends Day
    case object KnotHash               extends Day
    case object HexEd                  extends Day
    case object DigitalPlumber         extends Day
    case object PacketScanners         extends Day
  }

  val optParser: OptionParser[DayOpts] = new OptionParser[DayOpts]("AdventOfCode") {
    head("AdventOfCode", "2017")

    note("http://adventofcode.com/2017")

    def verifySamplesOpt =
      opt[Unit]("verify")
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

    cmd("day1")
      .action((_, c) => c.copy(day = Day.InverseCaptcha))
      .text("  Day 1: Inverse Captcha")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day2")
      .action((_, c) => c.copy(day = Day.CorruptionChecksum))
      .text("  Day 2: Corruption Checksum")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day3")
      .action((_, c) => c.copy(day = Day.SpiralMemory))
      .text("  Day 3: Spiral Memory")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day4")
      .action((_, c) => c.copy(day = Day.HighEntropyPassphrases))
      .text("  Day 4: High-Entropy Passphrases")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day5")
      .action((_, c) => c.copy(day = Day.TrampolineMaze))
      .text("  Day 5: A Maze of Twisty Trampolines, All Alike")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day6")
      .action((_, c) => c.copy(day = Day.MemoryReallocation))
      .text("  Day 6: Memory Reallocation")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day7")
      .action((_, c) => c.copy(day = Day.RecursiveCircus))
      .text("  Day 7: Recursive Circus")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day8")
      .action((_, c) => c.copy(day = Day.IHeardYouLikeRegisters))
      .text("  Day 8: I Heard You Like Registers")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day9")
      .action((_, c) => c.copy(day = Day.StreamProcessing))
      .text("  Day 9: Stream Processing")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day10")
      .action((_, c) => c.copy(day = Day.KnotHash))
      .text("  Day 10: Knot Hash")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day11")
      .action((_, c) => c.copy(day = Day.HexEd))
      .text("  Day 11: Hex Ed")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day12")
      .action((_, c) => c.copy(day = Day.DigitalPlumber))
      .text("  Day 12: Digital Plumber")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

    cmd("day13")
      .action((_, c) => c.copy(day = Day.PacketScanners))
      .text("  Day 13: Packet Scanners")
      .children(note(""), verifySamplesOpt, inputArg)
    note("")

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
  AdventOfCodeOpts.parse(args).foreach(_.run())
}
