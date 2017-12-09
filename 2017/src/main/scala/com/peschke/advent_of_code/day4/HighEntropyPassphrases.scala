package com.peschke.advent_of_code.day4

import scala.util.Try

import com.peschke.advent_of_code.AdventOfCodeDay
import com.peschke.advent_of_code.AdventOfCodeDay._

/**
  * http://adventofcode.com/2017/day/4
  * --- Day 4: High-Entropy Passphrases ---
  *
  * A new system policy has been put in place that requires all
  * accounts to use a passphrase instead of simply a password. A
  * passphrase consists of a series of words (lowercase letters)
  * separated by spaces.
  *
  * To ensure security, a valid passphrase must contain no duplicate
  * words.
  *
  * For example:
  * - aa bb cc dd ee is valid.
  * - aa bb cc dd aa is not valid - the word aa appears more than
  *   once.
  * - aa bb cc dd aaa is valid - aa and aaa count as different words.
  *
  * The system's full passphrase list is available as your puzzle
  * input. How many passphrases are valid?
  */
object HighEntropyPassphrases extends AdventOfCodeDay[Int, Unit] {

  def runDay1(input: String): scala.util.Try[Int] = Part1.validate(input)
  def runDay2(input: String): scala.util.Try[Unit] = Try(())

  def parse(input: String): Try[Seq[Passphrase]] = Try {
    input.split("\n").toSeq.map { rawPassphrase =>
      Passphrase(rawPassphrase.split(' ').toSeq)
    }
  }

  def verifySampleCases(): Unit = {
    println("Checking part 1 sample cases")
    Seq(
      "aa bb cc dd ee" -> 1,
      "aa bb cc dd aa" -> 0,
      "aa bb cc dd aaa" -> 1
    ).map((verifyResult(Part1.validate _) _).tupled).foreach(println)
  }
}
