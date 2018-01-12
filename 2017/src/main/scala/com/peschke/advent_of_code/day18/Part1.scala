package com.peschke.advent_of_code
package day18

import scala.util.Try

import cats.syntax.option._
import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.try_._

object Part1 {
  def parse(input: String): Try[Vector[OpCode]] =
    input
      .split('\n')
      .toVector
      .traverse(Parser.opCode.tryToParse(_))

  implicit class ValueOps(val value: Value) extends AnyVal {
    def resolve(implicit memory: Memory): Literal = value match {
      case l @ Literal(_) => l
      case r @ Register(_) => memory.lookup(r)
    }

    def render(implicit memory: Memory): String = value match {
      case Literal(v) => v.toString
      case Register(n) => s"$n(${resolve.value})"
    }
  }

  implicit class MemoryOps(val memory: Memory) extends AnyVal {
    def put(r: Register, v: Value): Memory =
      memory.copy(registers = memory.registers.updated(r, v.resolve(memory)))

    def update(r: Register)(f: Literal => Literal): Memory = put(r, f(lookup(r)))

    def lookup(r: Register): Literal =
      memory
        .registers
        .getOrElse(r, BigInt(0).literal)

    def recordSound(v: Value): Memory = memory.copy(lastSoundOpt = v.resolve(memory).some)

    def lastSound: Literal =
      memory
        .lastSoundOpt
        .getOrElse(throw new NoSuchElementException("No sound has been played"))

    def render: String =
      memory.registers.toSeq
        .map {
          case (Register(n), Literal(v)) => s"$n=$v"
        }
        .mkString("{", " ", "}") + s" ${memory.lastSoundOpt.map(_.value)}"
  }

  implicit class OpCodeOps(val opCode: OpCode) extends AnyVal {
    def render(implicit m: Memory): String = opCode match {
      case Snd(v)                  => s"snd ${v.render}"
      case Rcv(v)                => s"rcv ${v.render}"
      case Set(r, v)                 => s"set ${r.render} ${v.render}"
      case Add(r, v)                 => s"add ${r.render} ${v.render}"
      case Multiply(r, v)            => s"mul ${r.render} ${v.render}"
      case Modulo(r, v)              => s"mod ${r.render} ${v.render}"
      case JumpGreaterThanZero(r, v) => s"jgz ${r.render} ${v.render}"
    }
  }

  def execute(program: Vector[OpCode]): Try[Literal] = {
    val instructions = program.lift
    @scala.annotation.tailrec
    def loop(programCounter: Int, memory: Memory): Literal =
      instructions(programCounter) match {
        case None => memory.lastSound
        case Some(opCode) =>
          implicit val m: Memory = memory
          opCode match {
            case Snd(v) => loop(programCounter + 1, memory.recordSound(v))
            case Rcv(v) if v.resolve.value == BigInt(0) => loop(programCounter + 1, memory)
            case Rcv(_) =>
              println
              memory.lastSound
            case Set(r, v) => loop(programCounter + 1, memory.put(r, v))
            case Add(r, v) => loop(programCounter + 1, memory.update(r) { o =>
              (o.value + v.resolve.value).literal
            })
            case Multiply(r, v) => loop(programCounter + 1, memory.update(r) { o =>
              (o.value * v.resolve.value).literal
            })
            case Modulo(r, v) => loop(programCounter + 1, memory.update(r) { o =>
              (o.value % v.resolve.value).literal
            })
            case JumpGreaterThanZero(test, offset) if test.resolve.value > BigInt(0) =>
              loop(programCounter + offset.resolve.value.intValue, memory)
            case JumpGreaterThanZero(_, _) => loop(programCounter + 1, memory)
          }
      }
    Try(loop(0, Memory.empty))
  }

  def findFirstRecoveredSound(input: String): Try[Literal] =
    parse(input).flatMap(execute).mapError(Duet, input)
}
