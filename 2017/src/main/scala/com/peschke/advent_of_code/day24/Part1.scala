package com.peschke.advent_of_code
package day24

import cats.Eval
import cats.syntax.option._
import cats.syntax.traverse._
import cats.instances.vector._

import scala.util.Try

object Part1 {

  implicit class ComponentOps(val c: Component) extends AnyVal {
    def flip: Component = Component(c.rightPort, c.leftPort)
    def strength: Int = c.leftPort + c.rightPort
  }

  implicit class BridgeOps(val b: Bridge) extends AnyVal {
    def exposedPort: Int = b.parts.headOption.fold(0)(_.leftPort)

    def attemptAttachment(candidate: Component): Option[(Component, Bridge)] = {
      val port = exposedPort
      val componentOpt =
        if (candidate.rightPort == port) candidate.some
        else if (candidate.leftPort == port) candidate.flip.some
        else None
      componentOpt.map { c =>
        (candidate, Bridge(c :: b.parts))
      }
    }

    def strength: Int = b.parts.foldLeft(0)(_ + _.strength)
  }

  def createBridges(parts: Vector[Component]): Vector[Bridge] = {
    def loop(remaining: Set[Component], partial: Bridge): Eval[Vector[Bridge]] = {
      if (remaining.isEmpty) Eval.now(Vector(partial))
      else
        remaining
          .toVector
          .flatMap(partial.attemptAttachment)
          .flatTraverse[Eval, Bridge]{
          case (used, extended) =>
            Eval.defer {
              loop(remaining - used, extended).map(extended +: _)
            }
        }
    }

    loop(parts.toSet, Bridge(Nil)).value.sorted
  }

  def howStrongIsTheStrongestBridge(input: String): Try[Int] =
    Parser.parse(input)
      .map(createBridges)
      .map(_.map(_.strength).max)
      .mapError(ElectromagneticMoat, input)
}
