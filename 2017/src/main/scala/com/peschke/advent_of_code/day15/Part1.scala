package com.peschke.advent_of_code
package day15

import scala.util.Try

object Part1 {
  def parse(input: String): Try[Vector[Generator]] = Try {
    input
      .split('\n')
      .toVector
      .map { line =>
      line.split(' ') match {
        case Array("Generator", "A", "starts", "with", raw) =>
          Generator("A", BigInt(raw), BigInt(16807))

        case Array("Generator", "B", "starts", "with", raw) =>
          Generator("B", BigInt(raw), BigInt(48271))

        case _ => throw new IllegalArgumentException(s"Cannot parse input:\n$line")
      }
    }
  }

  val modulus: BigInt = BigInt("2147483647")
  val mask: BigInt = BigInt("1" * 16, 2)

  implicit class GeneratorOps(val gen: Generator) extends AnyVal {
    def generate: Iterator[BigInt] =
      Iterator
        .iterate(gen.seed)(prev => (prev * gen.factor) mod modulus)
        .drop(1)
  }

  implicit class JudgeOps(val judge: Judge) extends AnyVal {
    def evaluate: Int = {
      (judge.a.apply zip judge.b.apply)
        .map {
          case (a, b) if (a & judge.mask) == (b & judge.mask) => 1
          case _ => 0
        }
        .take(40e6.toInt)
        .sum
    }
  }

  def passJudgement(input: String): Try[BigInt] =
    parse(input)
      .map {
        case Seq(a, b) => Judge(() => a.generate, () => b.generate, mask).evaluate
        case gs =>
          throw new IllegalArgumentException(s"Expected exactly 2 generators, but was: $gs")
      }
}
