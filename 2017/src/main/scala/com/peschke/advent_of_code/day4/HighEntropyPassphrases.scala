package com.peschke.advent_of_code
package day4

import scala.util.Try

import com.peschke.advent_of_code.AdventOfCodeDay

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
  *
  * --- Part Two ---
  *
  * For added security, yet another system policy has been put in
  * place. Now, a valid passphrase must contain no two words that are
  * anagrams of each other - that is, a passphrase is invalid if any
  * word's letters can be rearranged to form any other word in the
  * passphrase.
  *
  * For example:
  *
  *  - abcde fghij is a valid passphrase.
  *  - abcde xyz ecdab is not valid - the letters from the third word
  *    can be rearranged to form the first word.
  *  - a ab abc abd abf abj is a valid passphrase, because all letters
  *    need to be used when forming another word.
  *  - iiii oiii ooii oooi oooo is valid.
  *  - oiii ioii iioi iiio is not valid - any of these words can be
  *    rearranged to form any other word.
  *
  * Under this new system policy, how many passphrases are valid?
  */
object HighEntropyPassphrases extends AdventOfCodeDay[Int, Int] {

  def runPart1(input: String): scala.util.Try[Int] = Part1.validate(input)
  def runPart2(input: String): scala.util.Try[Int] = Part2.validate(input)

  def parse(input: String): Try[Seq[Passphrase]] = Try {
    input.split("\n").toSeq.map { rawPassphrase =>
      Passphrase(rawPassphrase.split(' ').toSeq)
    }
  }

  def verifyPart1Samples(): Unit = {
    Seq(
      "aa bb cc dd ee" -> 1,
      "aa bb cc dd aa" -> 0,
      "aa bb cc dd aaa" -> 1
    ).map((verifyResult(Part1.validate _) _).tupled).foreach(println)
  }

  def verifyPart2Samples(): Unit = {
    Seq(
      "abcde fghij"              -> 1,
      "abcde xyz ecdab"          -> 0,
      "a ab abc abd abf abj"     -> 1,
      "iiii oiii ooii oooi oooo" -> 1,
      "oiii ioii iioi iiio"      -> 0
    ).map((verifyResult(Part2.validate _) _).tupled).foreach(println)
  }
}
