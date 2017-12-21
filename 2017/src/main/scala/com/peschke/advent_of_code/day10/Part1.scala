package com.peschke.advent_of_code
package day10

import scala.util.Try

object Part1 {
  implicit class HashContextOps[T](val hc: HashContext[T]) extends AnyVal {
    def subListIndexes(length: Int): Vector[Int] = {
      require(length <= hc.size, s"Lengths greater than buffer size are invalid: $length")
      Iterator.from(hc.position).take(length).toVector.map(hc.wrapIndex)
    }

    def extractSubList(length: Int): Vector[T] = subListIndexes(length).map(hc.buffer)

    def next(length: Int): HashContext[T] = {
      val indexes = subListIndexes(length)
      val replacements = indexes zip indexes.map(hc.buffer.apply).reverse
      HashContext(
        buffer = replacements.foldLeft(hc.buffer) {
          case (buffer, (index, value)) => buffer.updated(index, value)
        },
        position = hc.wrapIndex(hc.position + length + hc.skipSize),
        skipSize = hc.skipSize + 1)
    }
  }

  def parse(input: String): Try[List[Int]] = Try(input.split(',').map(_.toInt).toList)

  def verifyHash(input: String): Try[Int] =
    parse(input).map { input =>
      input.foldLeft(HashContext.default)(_ next _).values match {
        case a +: b +: _ => a * b
        case v => throw new NoSuchElementException(s"Result has less than two values: $v")
      }
    }.mapError(new KnotHashFailure(input, _))
}
