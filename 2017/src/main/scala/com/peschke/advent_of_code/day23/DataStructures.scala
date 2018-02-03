package com.peschke.advent_of_code.day23

import cats.{Eq, Show}
import cats.syntax.show._
import com.peschke.advent_of_code.day23.Meta.Label

sealed trait Value
case class Register(name: String) extends Value
case class Literal (value: BigInt) extends Value

trait ValueFallbackShow {
  implicit val rawShow: Show[Value] = Show.show {
    case Literal(v) => v.toString
    case Register(n) => n.toString
  }
}

object Value extends ValueFallbackShow {
  implicit val eq: Eq[Value] = Eq.fromUniversalEquals[Value]
  implicit def show(implicit memory: Memory): Show[Value] = Show.show {
    case Literal(v) => v.toString
    case r @ Register(n) =>
      val resolved = memory.registers.get(r).fold(BigInt(0))(_.value)
      s"$n($resolved)"
  }
}

sealed trait OpCode extends Product with Serializable
case class Set        (register: Register, value : Value) extends OpCode
case class Sub(register: Register, value : Value) extends OpCode
case class Multiply   (register: Register, value : Value) extends OpCode
case class JumpNotZero(test    : Value   , offset: Value) extends OpCode

trait OpCodeFallbackShow {
  implicit val rawShow: Show[OpCode] = Show.show {
    case Set(r, v)         => show"set $r $v"
    case Sub(r, v)         => show"sub $r $v"
    case Multiply(r, v)    => show"mul $r $v"
    case JumpNotZero(r, v) => show"jnz $r $v"
  }
}

object OpCode extends OpCodeFallbackShow {
  implicit val eq: Eq[OpCode] = Eq.fromUniversalEquals[OpCode]
  implicit def show(implicit memory: Memory): Show[OpCode] = Show.show {
    case Set(r, v)         => show"set $r $v"
    case Sub(r, v)         => show"sub $r $v"
    case Multiply(r, v)    => show"mul $r $v"
    case JumpNotZero(r, v) => show"jnz $r $v"
  }
}

sealed trait AST extends Product with Serializable
object AST {
  implicit val eq: Eq[AST] = Eq.fromUniversalEquals[AST]
  implicit val show: Show[AST] = Show.show(render(_))

  val Indent = "  "
  def render(exp: AST, padding: String = ""): String =
    exp match {
      case m: Meta => Meta.render(m, padding)
      case e: Expression => Expression.render(e, padding)
    }
}

sealed trait Meta extends AST with Product with Serializable
object Meta {
  case class Label(name: String) extends Meta
  case class Goto(label: Label) extends Meta

  def render(exp: Meta, padding: String = ""): String = exp match {
    case Label(name) => s"${padding}LABEL ${name.toUpperCase}:"
    case Goto(Label(name)) => s"${padding}goto ${name.toUpperCase}"
  }
}

sealed trait Expression extends AST with Product with Serializable
object Expression {
  case class Identity(v: Value) extends Expression

  case class Addition(lhs: Expression, rhs: Expression) extends Expression
  case class Subtraction(lhs: Expression, rhs: Expression) extends Expression
  case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
  case class Modulus(lhs: Expression, rhs: Expression) extends Expression

  case class Assignment(register: Register, rhs: Expression) extends Expression
  case class If(test: Conditional, ifTrue: Vector[AST], ifFalse: Vector[AST]) extends Expression
  case class DoWhile(test: Conditional, body: Vector[AST]) extends Expression

  case object Exit extends Expression

  case class Conditional(lhs: Expression, op: Conditional.Op, rhs: Expression) extends Expression
  object Conditional {
    sealed trait Op extends Product with Serializable
    case object Equals extends Op
    case object NotEquals extends Op

    val True: Conditional = Conditional(Identity(Literal(BigInt(1))), Equals, Identity(Literal(BigInt(1))))

    implicit val show: Show[Conditional.Op] = Show.show {
      case Equals => "=="
      case NotEquals => "!="
    }
  }

  def render(exp: Expression, padding: String = ""): String = exp match {
    case Identity(Register(n)) => s"$n"
    case Identity(Literal(v)) => s"$v"

    case Addition(lhs, rhs) => s"(${render(lhs)} + ${render(rhs)})"
    case Subtraction(lhs, rhs) => s"(${render(lhs)} - ${render(rhs)})"
    case Multiplication(lhs, rhs) => s"(${render(lhs)} * ${render(rhs)})"
    case Modulus(lhs, rhs) => s"(${render(lhs)} % ${render(rhs)})"

    case Assignment(Register(r), e) => s"$padding$r := ${render(e)}"

    case If(test, ifTrue, Vector()) =>
      val header = s"${padding}if (${render(test)}) {"
      val footer = s"$padding}"
      val body = ifTrue.map(AST.render(_, padding + AST.Indent))
      (header +: body :+ footer).mkString("\n")
    case If(test, Vector(), ifFalse) =>
      val header = s"${padding}if (${render(test)}) {"
      val footer = s"$padding}"
      val body = ifFalse.map(AST.render(_, padding + AST.Indent))
      (header +: body :+ footer).mkString("\n")
    case If(test, ifTrue, ifFalse) =>
      val header = s"${padding}if (${render(test)}) {"
      val sep    = s"$padding} else {"
      val footer = s"$padding}"
      val trueBody = ifTrue.map(AST.render(_, padding + AST.Indent))
      val falseBody = ifFalse.map(AST.render(_, padding + AST.Indent))
      ((header +: trueBody) ++ (sep +: falseBody :+ footer)).mkString("\n")

    case DoWhile(test, block) =>
      val header = s"${padding}do {"
      val footer = s"$padding} while (${render(test)})"
      val body = block.map(AST.render(_, padding + AST.Indent))
      (header +: body :+ footer).mkString("\n")

    case Exit => s"${padding}exit"

    case Conditional(lhs, op, rhs) => s"${render(lhs)} ${op.show} ${render(rhs)}"
  }

  implicit val show: Show[Expression] = Show.show(render(_: Expression))
}

case class Memory(registers: Map[Register, Literal], lastSoundOpt: Option[Literal])

object Memory {
  def empty: Memory = Memory(Map.empty, None)
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

  val set     : P[Set]      = P("set " ~/ register ~/ " " ~/ value).map(Set.tupled)
  val sub     : P[Sub]      = P("sub " ~/ register ~/ " " ~/ value).map(Sub.tupled)
  val multiply: P[Multiply] = P("mul " ~/ register ~/ " " ~/ value).map(Multiply.tupled)

  val jumpNotZero: P[JumpNotZero] =
    P("jnz " ~/ value ~/ " " ~/ value).map(JumpNotZero.tupled)

  val opCode: P[OpCode] =
    set | sub | multiply | jumpNotZero
}

object DecompileException {
  class CannotDecompileDynamicJumpsException(index: Int, opCode: OpCode) extends IllegalArgumentException {
    override def getMessage: String = s"Dynamic jump on line ${index + 1} (${opCode.show})"
  }

  class NoLabelForOffsetException(index: Int, opCode: OpCode, knownLabels: Map[Int, Label])
    extends IllegalArgumentException {
      def renderedLabels: String =
        knownLabels
          .toSeq
          .map(_.swap)
          .sortBy(_._1.name)
          .map {
            case (Label(n), i) => s"$n @ $i"
          }
          .mkString("\n")

      override def getMessage: String =
        s"""Jump offset on line ${index + 1} does not match any labels (${opCode.show}
           |Known labels:
           |$renderedLabels""".stripMargin
    }

  class IllegalJumpZeroException(index: Int, opCode: OpCode) extends IllegalArgumentException {
    override def getMessage: String = s"Jump with offset 0 on line ${index + 1} (${opCode.show})"
  }

  class BadChainAttemptException(baseIndex: Int, baseOpCode: OpCode, badIndex: Int, badOpCode: OpCode)
    extends IllegalArgumentException {
    override def getMessage: String =
      "Attempting to merge inappropriate opcode " +
      s"at line ${badIndex + 1} (${badOpCode.show} into opcode " +
      s"on line ${baseIndex + 1} (${baseOpCode.show}"
  }
}