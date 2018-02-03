package com.peschke.advent_of_code

package object day23 {
  implicit class BigIntToValueLift(val i: BigInt) extends AnyVal {
    def literal: Literal = Literal(i)
    def value: Value = Literal(i)
  }
}
