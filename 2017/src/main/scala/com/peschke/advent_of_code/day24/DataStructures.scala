package com.peschke.advent_of_code.day24

import cats.{Eq, Show}
import cats.syntax.show._
import cats.instances.int._

import scala.util.Try

case class Component(leftPort: Int, rightPort: Int)

object Component {
  implicit val eq: Eq[Component] = cats.derive.eq[Component]
  implicit val show: Show[Component] = Show.show {
    case Component(lhs, rhs) => s"$rhs/$lhs"
  }

  implicit def ordering(implicit intOrdering: Ordering[Int]): Ordering[Component] =
    (x: Component, y: Component) => (x, y) match {
      case (Component(xL, xR), Component(yL, yR)) =>
        intOrdering.compare(xL min xR, yL min yR) match {
          case 0 => intOrdering.compare(xL max xR, yL max yR)
          case v => v
        }
    }

  val Empty: Component = Component(0, 0)
}

case class Bridge(parts: List[Component])

object Bridge {
  implicit val eq: Eq[Bridge] = cats.derive.eq[Bridge]
  implicit val show: Show[Bridge] = Show.show {
    case Bridge(parts) => parts.reverseIterator.map(_.show).mkString(" -- ")
  }

  implicit def ordering(implicit componentOrdering: Ordering[Component]): Ordering[Bridge] =
    (x: Bridge, y: Bridge) =>
      x.parts
        .reverseIterator
        .zipAll(y.parts.reverseIterator, Component.Empty, Component.Empty)
        .foldLeft(0) {
          case (0, (a, b)) => componentOrdering.compare(a, b)
          case (v, _)      => v
        }
}

object Parser {
  def parse(input: String): Try[Vector[Component]] = Try {
    input.trim.split('\n').toVector.map { line =>
      val Array(lhs, rhs) = line.split('/')
      Component(lhs.trim.toInt, rhs.trim.toInt)
    }
  }
}