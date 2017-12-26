package com.peschke.advent_of_code
package day4

import scala.util.Try

object Part1 {
  implicit class PassphraseOps(val p: Passphrase) extends AnyVal {
    def isValid: Boolean = p.words.distinct == p.words
  }

  def validate(input: String): Try[Int] =
    HighEntropyPassphrases.parse(input).map { phrases =>
      phrases.count(_.isValid)
    }.mapError(HighEntropyPassphrases, input)
}
