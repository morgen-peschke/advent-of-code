package com.peschke.advent_of_code.day23

import scala.util.Try

import cats.syntax.traverse._

import cats.instances.vector._
import cats.instances.try_._

object Part1 {
  def parse(input: String): Try[Vector[OpCode]] =
    input
      .trim
      .split('\n')
      .toVector
      .traverse(Parser.opCode.tryToParse(_))

  implicit class ValueOps(val value: Value) extends AnyVal {
    def resolve(implicit memory: Memory): Literal = value match {
      case l @ Literal(_)  => l
      case r @ Register(_) => memory.lookup(r)
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
  }

  def execute(program: Vector[OpCode]): Try[Int] = {
    val instructions = program.lift

    @scala.annotation.tailrec
    def loop(mulCount: Int, programCounter: Int, memory: Memory): Int =
      instructions(programCounter) match {
        case None         => mulCount
        case Some(opCode) =>
          implicit val m: Memory = memory
          opCode match {
            case Set(r, v)      =>
              loop(mulCount, programCounter + 1, memory.put(r, v))
            case Sub(r, v)      =>
              loop(mulCount, programCounter + 1,
                memory.update(r) { o =>
                  (o.value - v.resolve.value).literal
                })
            case Multiply(r, v) =>
              loop(mulCount + 1, programCounter + 1,
                memory.update(r) { o =>
                  (o.value * v.resolve.value).literal
                })
            case JumpNotZero(test, offset)
              if test.resolve.value != BigInt(0) =>
              loop(mulCount, programCounter + offset.resolve.value.intValue, memory)
            case JumpNotZero(_, _)               =>
              loop(mulCount, programCounter + 1, memory)

            case _ => throw new IllegalArgumentException(s"Unsupported operation $opCode")
          }
      }

    Try(loop(0, 0, Memory.empty))
  }

  def debug(input: String): Try[Int] = parse(input.trim).flatMap(execute)
}
