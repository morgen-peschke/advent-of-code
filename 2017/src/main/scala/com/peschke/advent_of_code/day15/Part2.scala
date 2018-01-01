package com.peschke.advent_of_code
package day15

import scala.util.Try

object Part2 {
  import Part1.{mask, GeneratorOps}

  def parse(input: String): Try[Seq[GeneratorWithFilter]] = Try {
    input.split('\n').map { line =>
      line.split(' ') match {
        case Array("Generator", "A", "starts", "with", raw) =>
          GeneratorWithFilter(Generator("A", BigInt(raw), BigInt(16807)), BigInt(4))

        case Array("Generator", "B", "starts", "with", raw) =>
          GeneratorWithFilter(Generator("B", BigInt(raw), BigInt(48271)), BigInt(8))

        case _ => throw new IllegalArgumentException(s"Cannot parse input:\n$line")
      }
    }
  }

  val Zero: BigInt = BigInt(0)

  implicit class GeneratorWithFilterOps(val gen: GeneratorWithFilter) extends AnyVal {
    def generate: Iterator[BigInt] =
      gen.base.generate.filter(v => (v mod gen.filterMod) == Zero)
  }

  implicit class JudgeOps(val judge: Judge) extends AnyVal {
    def evaluate: Int = {
      (judge.a.apply zip judge.b.apply)
        .map {
          case (a, b) if (a & judge.mask) == (b & judge.mask) => 1
          case _ => 0
        }
        .take(5e6.toInt)
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
