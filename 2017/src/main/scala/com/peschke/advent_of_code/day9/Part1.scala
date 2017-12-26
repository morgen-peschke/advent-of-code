package com.peschke.advent_of_code
package day9

import cats.syntax.option._

import scala.util.{Try, Failure, Success}

object Part1 {
  private val printLog: Boolean = false
  private def log(str: => String): Unit = {
    if (printLog) {
      println(str)
    }
  }

  def parseGarbage(stream: List[Char]): Option[(Chunk.Garbage, List[Char])] = {
    def loop(buffer: List[Char], accum: Vector[Char]): Option[(Chunk.Garbage, List[Char])] =
      buffer match {
        case '!' :: _ :: tail => loop(tail, accum)
        case '>' :: tail => (Chunk.Garbage(accum.mkString) -> tail).some
        case Nil =>
          log(s"Encountered end of stream while extracting Garbage from ${stream.mkString}")
          None
        case c :: tail => loop(tail, accum :+ c)
      }

    stream match {
      case '<' :: rest => loop(rest, Vector.empty[Char])
      case _ =>
        log(s"Expected '<' at ${stream.mkString}")
        None
    }
  }

  def parseGroup(stream: List[Char]): Option[(Chunk.Group, List[Char])] = {
    def loop(buffer: List[Char], accum: Vector[Chunk]): Option[(Chunk.Group, List[Char])] =
      buffer match {
        case '!' :: _ :: tail => loop(tail, accum)
        case ',' :: tail => loop(tail, accum)
        case '}' :: tail => (Chunk.Group(accum) -> tail).some
        case Nil =>
          log(s"Encountered end of stream while extracting Group from ${stream.mkString}")
          None
        case tail => parseGarbage(tail).orElse(parseGroup(tail)).flatMap {
          case (chunk, rest) => loop(rest, accum :+ chunk)
        }
      }

    stream match {
      case '{' :: rest => loop(rest, Vector.empty[Chunk])
      case _ =>
        log(s"Expected '{' at ${stream.mkString}")
        None
    }
  }

  def parse(input: String): Try[Chunk] = {
    val stream = input.toList
    parseGroup(stream).orElse(parseGarbage(stream)) match {
      case Some((group, Nil)) => Success(group)
      case Some((_, tail)) => Failure(
        new IllegalArgumentException(s"Unexpected end of chunk at: ${tail.mkString}"))
      case None => Failure(new IllegalArgumentException(s"Unable to parse chunk from: $input"))
    }
  }

  def totalScore(input: String): Try[Int] = {
    def calculateScore(chunk: Chunk, depth: Int = 1): Int = chunk match {
      case Chunk.Garbage(_) => 0
      case Chunk.Group(contents) =>
        contents.map(calculateScore(_: Chunk, depth + 1)).sum + depth
    }
    parse(input).map(calculateScore(_: Chunk)).mapError(StreamProcessing, input)
  }
}
