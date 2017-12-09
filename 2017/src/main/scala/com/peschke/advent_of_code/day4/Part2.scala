package com.peschke.advent_of_code.day4

import scala.util.{Try, Failure}

import com.peschke.advent_of_code.AdventOfCodeDay._

object Part2 {
  implicit class PassphraseOps(val p: Passphrase) extends AnyVal {
    def isValid: Boolean = {
      val normalized_words = p.words.map(_.sorted)
      normalized_words.distinct == normalized_words
    }
  }

  def validate(input: String): Try[Int] =
    HighEntropyPassphrases.parse(input).map { phrases =>
      phrases.count(_.isValid)
    }.wrapFailure(throwable => Failure(new HighEntropyPassphrasesFailure(input, throwable)))
}
