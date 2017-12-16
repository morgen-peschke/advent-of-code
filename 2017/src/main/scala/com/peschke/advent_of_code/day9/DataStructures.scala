package com.peschke.advent_of_code
package day9

class StreamProcessingFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"StreamProcessing failed on input:\n$input", cause)

sealed trait Chunk
object Chunk {
  case class Garbage(contents: String) extends Chunk

  case class Group(contents: Vector[Chunk]) extends Chunk

  object Group {
    val empty: Group = Group(Vector.empty[Chunk])
  }

  def garbage(contents: String): Chunk = Garbage(contents)
  def group(cv: Vector[Chunk]): Chunk = Group(cv)
  def group(c: Chunk, cs: Chunk*): Chunk = Group((c +: cs).toVector)
  def group: Chunk = Group(Vector.empty)
}
