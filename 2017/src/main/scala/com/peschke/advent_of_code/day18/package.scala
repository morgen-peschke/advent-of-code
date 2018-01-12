package com.peschke.advent_of_code

package object day18 {
  implicit class StringToValueLift(val s: String) extends AnyVal {
    def register: Register = Register(s)
    def value: Value = Register(s)
  }

  implicit class BigIntToValueLift(val i: BigInt) extends AnyVal {
    def literal: Literal = Literal(i)
    def value: Value = Literal(i)
  }

  implicit class ValueOpCodeSyntax(val v: Value) extends AnyVal {
    def snd: Snd = Snd(v)
    def ifGreaterThanZeroJump(offset: Value): JumpGreaterThanZero =
      JumpGreaterThanZero(v, offset)
  }

  implicit class RegisterOpCodeSyntax(val r: Register) extends AnyVal {
    def rcv: Rcv = Rcv(r)
    def := (v: Value): Set = Set(r, v)
    def += (v: Value): Add = Add(r, v)
    def *= (v: Value): Multiply = Multiply(r, v)
    def %= (v: Value): Modulo = Modulo(r, v)
  }
}
