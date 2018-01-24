package com.peschke.advent_of_code
package day9

import cats.{Eq, Show}
import cats.syntax.eq._
import cats.instances.vector._
import cats.instances.string._

sealed trait Chunk
object Chunk {
  case class Garbage(contents: String) extends Chunk
  object Garbage {
    implicit val eq: Eq[Garbage] = cats.derive.eq[Garbage]
    implicit val show: Show[Garbage] = cats.derive.show[Garbage]
  }

  case class Group(contents: Vector[Chunk]) extends Chunk

  object Group {
    val empty: Group = Group(Vector.empty[Chunk])
    implicit val eq: Eq[Group] = cats.derive.eq[Group]
    implicit val show: Show[Group] = cats.derive.show[Group]
  }

  def garbage(contents: String): Chunk = Garbage(contents)
  def group(cv: Vector[Chunk]): Chunk = Group(cv)
  def group(c: Chunk, cs: Chunk*): Chunk = Group((c +: cs).toVector)
  def group: Chunk = Group(Vector.empty)

  implicit val eq: Eq[Chunk] = Eq.instance {
    case (x @ Garbage(_), y @ Garbage(_)) => x === y
    case (x @ Group(_), y @ Group(_)) => x === y
    case _ => false
  }
}
