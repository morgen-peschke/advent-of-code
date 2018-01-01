package com.peschke.advent_of_code
package day14

import scala.util.Try
import com.peschke.advent_of_code.day10.Part2.hash
import com.peschke.advent_of_code.day10.RenderedHash

import cats.instances.try_._
import cats.instances.list._
import cats.syntax.traverse._

object Part1 {
  def hashToBigInt(rh: RenderedHash): Try[BigInt] = Try(BigInt(rh.hash, 16))

  def usedSquares(input: String): Try[Int] =
    (0 to 127).toList
      .traverse { row =>
        for {
          renderedHash <- hash(s"$input-$row")
          bigInt <- hashToBigInt(renderedHash)
        } yield bigInt.bitCount
      }
      .map(_.sum)
}
