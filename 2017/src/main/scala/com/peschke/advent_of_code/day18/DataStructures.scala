package com.peschke.advent_of_code
package day18

case class ProgramCounter(value: BigInt)

sealed trait Value
case class Register(name: String) extends Value
case class Literal (value: BigInt) extends Value

sealed trait OpCode
case class Snd                (value   : Value                  ) extends OpCode
case class Rcv                (test    : Register               ) extends OpCode
case class Set                (register: Register, value : Value) extends OpCode
case class Add                (register: Register, value : Value) extends OpCode
case class Multiply           (register: Register, value : Value) extends OpCode
case class Modulo             (register: Register, value : Value) extends OpCode
case class JumpGreaterThanZero(test    : Value   , offset: Value) extends OpCode

case class Memory(registers: Map[Register, Literal], lastSoundOpt: Option[Literal])

object Memory {
  def empty: Memory = Memory(Map.empty, None)
}

case class Program(
  programCounter: ProgramCounter,
  memory: Map[Register, Literal],
  pipe: Vector[Literal])

object Program {
  def initialize(id: BigInt): Program =
    Program(
      ProgramCounter(BigInt(0)),
      Map("p".register -> id.literal),
      Vector.empty)
}

object Parser {
  import fastparse.all._

  val register: P[Register] = P(CharIn('a' to 'z').! | CharIn('A' to 'Z').!).map(Register)
  val literal : P[Literal]  = P("-".!.? ~ CharIn('0' to '9').rep.!).map {
    case (maybeNeg, digits) =>
      val sign = maybeNeg.fold("")(identity)
      Literal(BigInt(s"$sign$digits"))
  }
  val value: P[Value] = register | literal

  val snd     : P[Snd]      = P("snd " ~/ value).map(Snd)
  val rcv     : P[Rcv]      = P("rcv " ~/ register).map(Rcv)
  val set     : P[Set]      = P("set " ~/ register ~/ " " ~/ value).map(Set.tupled)
  val add     : P[Add]      = P("add " ~/ register ~/ " " ~/ value).map(Add.tupled)
  val multiply: P[Multiply] = P("mul " ~/ register ~/ " " ~/ value).map(Multiply.tupled)
  val modulo  : P[Modulo]   = P("mod " ~/ register ~/ " " ~/ value).map(Modulo.tupled)

  val jumpGreaterThanZero: P[JumpGreaterThanZero] =
    P("jgz " ~/ value ~/ " " ~/ value).map(JumpGreaterThanZero.tupled)

  val opCode: P[OpCode] =
    snd | set | add | multiply | modulo | rcv | jumpGreaterThanZero
}
