package com.peschke.advent_of_code
package day18

import cats.syntax.option._

import scala.util.Try

object Part2 {
  implicit class ValueOps(val value: Value) extends AnyVal {
    def resolve(implicit context: Program): Literal = value match {
      case l @ Literal(_) => l
      case r @ Register(_) => context.lookup(r)
    }

    def render(implicit context: Program): String = value match {
      case Literal(v) => v.toString
      case Register(n) => s"$n(${resolve.value})"
    }
  }

  implicit class ProgramOps(val program: Program) extends AnyVal {
    def put(r: Register, v: Value): Program =
      program.copy(memory = program.memory.updated(r, v.resolve(program)))

    def update(r: Register)(f: Literal => Literal): Program = put(r, f(lookup(r)))

    def lookup(r: Register): Literal =
      program
        .memory
        .getOrElse(r, BigInt(0).literal)

    def enqueue(vs: Vector[Literal]): Program =
      program.copy(pipe = program.pipe ++ vs)

    def receive: (Option[Literal], Vector[Literal]) = program.pipe match {
      case Vector() => (None,    Vector.empty)
      case v +: vs  => (v.some, vs)
    }

    def render: String = {
      val memory = program.memory.toSeq
        .map {
          case (Register(n), Literal(v)) => s"$n=$v"
        }
        .mkString("{", " ", "}")
      val pipe = program.pipe.map(_.value).mkString(" << ")
      s"$memory | $pipe"
    }

    def jump(offset: BigInt): Program =
      program.copy(programCounter = ProgramCounter(program.programCounter.value + offset))

    def step: Program = jump(BigInt(1))
  }

  implicit class OpCodeOps(val opCode: OpCode) extends AnyVal {
    def render(implicit context: Program): String = opCode match {
      case Snd(v)                    => s"snd ${v.render}"
      case Rcv(v)                    => s"rcv ${v.render}"
      case Set(r, v)                 => s"set ${r.render} ${v.render}"
      case Add(r, v)                 => s"add ${r.render} ${v.render}"
      case Multiply(r, v)            => s"mul ${r.render} ${v.render}"
      case Modulo(r, v)              => s"mod ${r.render} ${v.render}"
      case JumpGreaterThanZero(r, v) => s"jgz ${r.render} ${v.render}"
    }
  }

  def execute(program: Vector[OpCode]): Int = {
    val loadInstruction: Program => Option[OpCode] =
      ((_: Program).programCounter.value.intValue) andThen program.lift

    def isBlocked(context: Program): Boolean =
      loadInstruction(context) match {
        case None => true
        case Some(Rcv(_)) => context.pipe.isEmpty
        case Some(_) => false
      }

    @scala.annotation.tailrec
    def runUntilBlocked(
      context: Program,
      sent: Vector[Literal] = Vector.empty
    ): (Program, Vector[Literal]) =
      loadInstruction(context) match {
        case None => (context, sent)

        case Some(opCode) =>
          implicit val c: Program = context
          opCode match {
            case Rcv(r) =>
              val (valueOpt, remaining) = context.receive
              valueOpt match {
                case None    => (context, sent)
                case Some(v) =>
                  runUntilBlocked(
                    context.copy(pipe = remaining).put(r, v).step,
                    sent)
              }
            case Snd(v) =>
              runUntilBlocked(context.step, sent :+ v.resolve)
            case Set(r, v) =>
              runUntilBlocked(
                context.step.put(r, v.resolve),
                sent)
            case Add(r, v) =>
              runUntilBlocked(
                context.step.update(r)(o => (o.value + v.resolve.value).literal),
                sent)
            case Multiply(r, v) =>
              runUntilBlocked(
                context.step.update(r)(o => (o.value * v.resolve.value).literal),
                sent)
            case Modulo(r, v) =>
              runUntilBlocked(
                context.step.update(r)(o => (o.value % v.resolve.value).literal),
                sent)
            case JumpGreaterThanZero(test, offset) if test.resolve.value > BigInt(0) =>
              runUntilBlocked(
                context.jump(offset.resolve.value),
                sent)
            case JumpGreaterThanZero(_, _) =>
              runUntilBlocked(context.step, sent)
          }
      }

    @scala.annotation.tailrec
    def loop(
      zero: Program,
      one: Program,
      messageCountFromOne: Int
    ): Int =
      if (isBlocked(zero) && isBlocked(one)) messageCountFromOne
      else {
        val (nextZero, sentFromZero) = runUntilBlocked(zero)
        val (nextOne, sentFromOne) = runUntilBlocked(one.enqueue(sentFromZero))
        loop(nextZero.enqueue(sentFromOne), nextOne, messageCountFromOne + sentFromOne.size)
      }

    loop(
      Program.initialize(BigInt(0)),
      Program.initialize(BigInt(1)),
      0)
  }

  def countMessages(input: String): Try[Int] =
    Part1.parse(input).map(execute).mapError(Duet, input)
}
