package com.peschke.advent_of_code
package day8

import cats.kernel.Order

import cats.syntax.either._
import cats.syntax.validated._

import cats.instances.vector._
import cats.instances.int._

import scala.util.Try

class IHeardYouLikeRegistersFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"IHeardYouLikeRegisters failed on input:\n$input", cause)

case class LineLocation(line: Int) {
  def desc: String = s"[Line $line]"
  def tokenNumber(index: Int): TokenLocation = TokenLocation(line, index)
}

case class TokenLocation(line: Int, index: Int) {
  def desc: String = s"[Line $line, token $index]"
  def offset(delta: Int): TokenLocation = copy(index = index + delta)
}

case class Token(value: String) {
  override def toString: String = s""""$value""""
}
object Token {
  def tokenize(line: String, loc: LineLocation): ErrorOr[List[Token]] =
    if (line.contains("\n"))
      s"${loc.desc} Unable to tokenize: multi-line input".asLeft[List[Token]]
    else if (line.forall(_.isWhitespace))
      s"${loc.desc} Unable to tokenize: input is empty".asLeft[List[Token]]
    else line.trim.split("\\s+").map(Token(_)).toList.asRight[Error]
}

case class Register(name: String)
object Register {
  def parse(t: Token, loc: TokenLocation): ErrorOr[Register] = {
    def error(errorDesc: String) =
      s"${loc.desc} Expected Register but $errorDesc: $t".asLeft[Register]

    t.value match {
      case "inc" | "dec" => error("appears to be an Operation")
      case v if !v.forall(_.isLetter) => error("contains illegal characters")
      case v => Register(v).asRight[Error]
    }
  }
}

case class Amount(value: Int)
object Amount {
  def parse(t: Token, loc: TokenLocation): ErrorOr[Amount] =
    Try(t.value.toInt).fold(
      ex => s"${loc.desc} Unable to parse as integer: $t (${ex.getMessage})".asLeft[Amount],
      i => Amount(i).asRight[Error])

  implicit val order: Order[Amount] = Order.by(_.value)
}

case class Operation(register: Register, opCode: Operation.OpCode, amount: Amount)
object Operation {
  sealed trait OpCode
  object OpCode {
    case object Increment extends OpCode
    case object Decrement extends OpCode

    def parse(t: Token, loc: TokenLocation): ErrorOr[OpCode] =
      t.value match {
        case "inc" => Increment.asRight[Error]
        case "dec" => Decrement.asRight[Error]
        case _ => s"${loc.desc} Expected Operation.Opcode, but was $t".asLeft[OpCode]
      }
  }

  def parse(ts: List[Token], loc: TokenLocation): ErrorOr[(Operation, List[Token], TokenLocation)] =
    ts match {
      case regToken :: opCodeToken :: amountToken :: rest =>
        for {
          reg <- Register.parse(regToken, loc)
          opCode <- OpCode.parse(opCodeToken, loc.offset(1))
          amount <- Amount.parse(amountToken, loc.offset(2))
        } yield (Operation(reg, opCode, amount), rest, loc.offset(3))

      case Nil =>
        s"${loc.desc} Expected Operation, but reached end of input"
          .asLeft[(Operation, List[Token], TokenLocation)]

      case _ =>
        s"${loc.desc} Not enough tokens to parse an Operation, was $ts"
          .asLeft[(Operation, List[Token], TokenLocation)]
    }
}

case class Conditional(register: Register, cmp: Conditional.Comparison, amount: Amount)
object Conditional {
  sealed trait Comparison {
    def or(other: Comparison): Comparison = Comparison.Or(this, other)
  }
  object Comparison {
    case object EqualTo extends Comparison
    case object NotEqualTo extends Comparison
    case object LessThan extends Comparison
    case object GreaterThan extends Comparison
    case class Or(c1: Comparison, c2: Comparison) extends Comparison

    def parse(t: Token, loc: TokenLocation): ErrorOr[Comparison] = t.value match {
      case "==" => EqualTo.asRight[Error]
      case "!=" => NotEqualTo.asRight[Error]
      case ">"  => GreaterThan.asRight[Error]
      case "<"  => LessThan.asRight[Error]
      case ">=" => (GreaterThan or EqualTo).asRight[Error]
      case "<=" => (LessThan or EqualTo).asRight[Error]
      case _ => s"${loc.desc} Expected Conditional.Comparison but was $t".asLeft[Comparison]
    }
  }

  def parse(ts: List[Token], loc: TokenLocation): ErrorOr[(Conditional, List[Token], TokenLocation)] =
    ts match {
      case Token("if") :: regToken :: cmpToken :: amountToken :: rest =>
        for {
          reg <- Register.parse(regToken, loc.offset(1))
          cmp <- Comparison.parse(cmpToken, loc.offset(2))
          amount <- Amount.parse(amountToken, loc.offset(3))
        } yield (Conditional(reg, cmp, amount), rest, loc.offset(4))

      case Nil =>
        s"${loc.desc} Expected Conditional, but reached end of input"
          .asLeft[(Conditional, List[Token], TokenLocation)]

      case _ =>
        s"${loc.desc} Not enough tokens to parse a Conditional, was $ts"
          .asLeft[(Conditional, List[Token], TokenLocation)]
    }
}

case class Instruction(op: Operation, conditional: Conditional)
object Instruction {
  private def ensureEndOfString(ts: List[Token], loc: TokenLocation): ErrorOr[Unit] = ts match {
    case Nil => ().asRight[Error]
    case _ => s"${loc.desc} Expected end of input, but was $ts".asLeft[Unit]
  }

  def parse(ts: List[Token], loc: LineLocation): ErrorOr[Instruction] =
    for {
      opAndRest <- Operation.parse(ts, loc.tokenNumber(1))
      condAndLeftOver <- Conditional.parse(opAndRest._2, opAndRest._3)
      _ <- ensureEndOfString(condAndLeftOver._2, condAndLeftOver._3)
    } yield Instruction(opAndRest._1, condAndLeftOver._1)

  def compile(input: String): CompileErrorsOr[List[Instruction]] =
    input
      .split('\n')
      .zipWithIndex
      .map {
        case (line, index) =>
          val loc = LineLocation(index + 1)
          (for {
            tokens <- Token.tokenize(line, loc)
            instruction <- Instruction.parse(tokens, loc)
          } yield Vector(instruction)).leftMap(_ + s" in: $line")
      }
      .map(_.toValidatedNel)
      .foldLeft((Vector.empty[Instruction]).valid[CompileErrors])(_ combine _)
      .map(_.toList)
}
