package com.peschke.advent_of_code
package day10

import scala.util.Try

import Part1.HashContextOps

object Part2 {
  implicit class IntHashContextOps(val hc: HashContext[Int]) extends AnyVal {
    def asSparseHash: SparseHash = SparseHash(hc.values)

    def singleRound(lengths: Vector[Int]): HashContext[Int] =
      lengths.foldLeft(hc)(_ next _)
  }

  implicit class SparseHashOps(val sh: SparseHash) extends AnyVal {
    def asDenseHash: DenseHash = DenseHash(sh.bytes.grouped(16).map(_.reduce(_ ^ _)).toVector)
  }

  implicit class DenseHashOps(val dh: DenseHash) extends AnyVal {
    def render: RenderedHash =
      dh.bytes.map(_.toHexString.reverse.padTo(2, '0').reverse).mkString.isHash
  }

  implicit class StringOps(val s: String) extends AnyVal {
    def isHash: RenderedHash = RenderedHash(s)
  }

  def parse(input: String): Try[Vector[Int]] = Try {
    input.getBytes.toVector.map(_.toInt) ++ Vector(17, 31, 73, 47, 23)
  }

  def hash(input: String): Try[RenderedHash] =
    parse(input)
      .map { lengths =>
      Vector.fill(64)(lengths)
        .foldLeft(HashContext.default)(_ singleRound _)
        .asSparseHash
        .asDenseHash
        .render
      }
      .mapError(new KnotHashFailure(input, _))
}
