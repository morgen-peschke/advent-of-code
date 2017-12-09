package com.peschke.advent_of_code.day4

class HighEntropyPassphrasesFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"HighEntropyPassphrases failed on input:\n$input", cause)

case class Passphrase(val words: Seq[String])
