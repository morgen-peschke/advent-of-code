package com.peschke.advent_of_code

import cats.syntax.either._

package object day16 {
  implicit class StringLiftOps(val s: String) extends AnyVal {
    def asChar: Either[String, Char] =
      s.toList match {
        case c :: Nil => c.asRight
        case _ => s"Expected a single char, was <$s>".asLeft
      }
  }
}
