package com.peschke.advent_of_code.day4

import scala.util.{Try, Failure}

import com.peschke.advent_of_code.AdventOfCodeDay._

object Part1 {
  implicit class PassphraseOps(val p: Passphrase) extends AnyVal {
    def isValid: Boolean = p.words.distinct == p.words
  }

  def validate(input: String): Try[Int] =
    HighEntropyPassphrases.parse(input).map { phrases =>
      phrases.count(_.isValid)
    }.wrapFailure(throwable => Failure(new HighEntropyPassphrasesFailure(input, throwable)))
}
