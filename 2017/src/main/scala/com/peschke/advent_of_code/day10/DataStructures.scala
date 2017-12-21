package com.peschke.advent_of_code
package day10

class KnotHashFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"KnotHash failed on input:\n$input", cause)

case class CircularBuffer[T](buffer: Vector[T]) extends (Int => T) {
  def wrapIndex(rawIndex: Int): Int = {
    require(rawIndex >= 0, "Negative indexes are not supported")
    rawIndex % buffer.size
  }

  def apply(index: Int): T = buffer(wrapIndex(index))

  def updated(index: Int, element: T): CircularBuffer[T] =
    CircularBuffer(buffer.updated(wrapIndex(index), element))

  def size: Int = buffer.size

  private def renderBuffer: String = buffer.mkString(",")

  override def toString: String = s"CircularBuffer($renderBuffer)"
}

object CircularBuffer {
  def fill[T](size: Int)(element: T): CircularBuffer[T] =
    CircularBuffer(Vector.fill(size)(element))

  def containing[T](e: T, es: T*): CircularBuffer[T] =
    CircularBuffer(e +: es.toVector)
}

case class HashContext[T](buffer: CircularBuffer[T], position: Int = 0, skipSize: Int = 0) {
  def size: Int = buffer.size
  def wrapIndex(i: Int): Int = buffer.wrapIndex(i)
  def values: Vector[T] = buffer.buffer
}

object HashContext {
  def default: HashContext[Int] =
    HashContext[Int](CircularBuffer((0 to 255).toVector))
}

case class SparseHash(bytes: Vector[Int]) {
  require(bytes.length == 256, s"Sparse hash must be 256 bytes long: $bytes")
}

case class DenseHash(bytes: Vector[Int]) {
  require(bytes.length == 16, s"Dense hash must be 16 bytes long: $bytes")
}

case class RenderedHash(hash: String) extends AnyVal {
  override def toString: String = hash
}
