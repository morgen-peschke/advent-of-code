package com.peschke.advent_of_code
package day9

import scala.util.Try

object Part2 {
  def garbageLength(input: String): Try[Int] = {
    def calculateLength(chunk: Chunk): Int = chunk match {
      case Chunk.Garbage(content) => content.length
      case Chunk.Group(contents) => contents.map(calculateLength).sum
    }
    Part1.parse(input).map(calculateLength).mapError(new StreamProcessingFailure(input, _))
  }
}
