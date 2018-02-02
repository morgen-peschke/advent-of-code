package com.peschke.advent_of_code
package day23

import cats.Eval
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import com.peschke.advent_of_code.day23.DecompileException.{BadChainAttemptException, CannotDecompileDynamicJumpsException, IllegalJumpZeroException, NoLabelForOffsetException}
import com.peschke.advent_of_code.day23.Expression.Conditional.{Equals, NotEquals, True}
import com.peschke.advent_of_code.day23.Expression.{Addition, Assignment, Conditional, DoWhile, Exit, Identity, If, Modulus, Multiplication, Subtraction}
import com.peschke.advent_of_code.day23.Meta.{Goto, Label}
import com.peschke.advent_of_code.day23.Part1.parse

import scala.annotation.tailrec
import scala.util.Try

object Part2 {

  implicit class LiteralOps(val literal: Literal) extends AnyVal {
    def + (other: Literal): Literal = Literal(literal.value + other.value)
    def - (other: Literal): Literal = Literal(literal.value - other.value)
    def * (other: Literal): Literal = Literal(literal.value * other.value)
    def % (other: Literal): Literal = Literal(literal.value % other.value)
  }

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
      (r, memory.registers.get(r)) match {
        case (Register("a"), None) => BigInt(1).literal
        case (_, None)             => BigInt(0).literal
        case (_, Some(l))          => l
      }
  }

  implicit class ASTOps(val ast: AST) extends AnyVal {
    def isLabel: Boolean = ast match {
      case Label(_) => true
      case _        => false
    }
  }

  private val Zero = BigInt(0)
  private val One = BigInt(1)
  //private val Two = BigInt(2)

  def decompile(assembly: Vector[OpCode]): Try[Vector[AST]] = Try {
    val lastInstruction = assembly.size - 1

    val labels: Map[Int, Label] =
      assembly.zipWithIndex.foldLeft(Map.empty[Int, Label]) {
        case (_, (opCode @ JumpNotZero(_, Register(_)), index)) =>
          throw new CannotDecompileDynamicJumpsException(index, opCode)

        case (knownLabels, (JumpNotZero(_, Literal(offset)), index)) =>
          val destination = index + offset.toInt
          if (knownLabels.contains(destination) || destination > lastInstruction)
            knownLabels
          else knownLabels + (destination -> Label(s"L${knownLabels.size + 1}"))
        case (knownLabels, _)                                        => knownLabels
      }

    class WithLabelHelper(index: Int) {
      def apply(code: AST): Vector[AST] =
        labels.get(index).fold(Vector(code))(_ +: Vector(code))

      def apply(code: Option[AST]): Vector[AST] =
        labels.get(index).fold(code.toVector)(_ +: code.toVector)
    }

    def withLabel(index: Int): WithLabelHelper = new WithLabelHelper(index)

    def isChainableInto(targetRegister: Register): ((OpCode, Int)) => Boolean = {
      case (Sub(r, _), i)      => r == targetRegister && !labels.contains(i)
      case (Multiply(r, _), i) => r == targetRegister && !labels.contains(i)
      case _                   => false
    }

    def buildRHSChain(baseIndex: Int,
                      baseOpCode: OpCode,
                      baseLHS: Expression,
                      chain: Vector[(OpCode, Int)]): Expression =
      chain.foldLeft(baseLHS) { (lhs, rhs) =>
        rhs match {
          case (Sub(_, Literal(v)), _) if v < Zero =>
            Addition(lhs, Identity(Literal(-v)))
          case (Sub(_, v), _)                      =>
            Subtraction(lhs, Identity(v))
          case (Multiply(_, v), _)                 =>
            Multiplication(lhs, Identity(v))
          case (opCode, index)                     =>
            throw new BadChainAttemptException(
              baseIndex = baseIndex,
              baseOpCode = baseOpCode,
              badIndex = index,
              badOpCode = opCode)
        }
      }

    @tailrec
    def loop(statements: Vector[(OpCode, Int)], sourceCode: Vector[AST]): Vector[AST] = statements match {
      case Vector() => sourceCode

      case (opcode @ Set(register, value), index) +: tail =>
        val (chainableOps, rest) = tail.span(isChainableInto(register))
        loop(rest, sourceCode ++ withLabel(index) {
          Assignment(
            register,
            buildRHSChain(index, opcode, Identity(value), chainableOps))
        })

      case (opcode @ Sub(register, Literal(v)), index) +: tail if v < Zero =>
        val (chainableOps, rest) = tail.span(isChainableInto(register))
        loop(rest, sourceCode ++ withLabel(index) {
          Assignment(
            register,
            buildRHSChain(
              index,
              opcode,
              Addition(Identity(register), Identity(Literal(-v))),
              chainableOps))
        })

      case (opcode @ Sub(register, value), index) +: tail =>
        val (chainableOps, rest) = tail.span(isChainableInto(register))
        loop(rest, sourceCode ++ withLabel(index) {
          Assignment(
            register,
            buildRHSChain(
              index,
              opcode,
              Subtraction(Identity(register), Identity(value)),
              chainableOps))
        })

      case (opcode @ Multiply(register, value), index) +: tail =>
        val (chainableOps, rest) = tail.span(isChainableInto(register))
        loop(rest, sourceCode ++ withLabel(index) {
          Assignment(
            register,
            buildRHSChain(
              index,
              opcode,
              Multiplication(Identity(register), Identity(value)),
              chainableOps))
        })

      case (opCode @ JumpNotZero(_, Register(_)), index) +: _ =>
        throw new CannotDecompileDynamicJumpsException(index, opCode)

      case (opCode @ JumpNotZero(_, Literal(Zero)), index) +: _ =>
        throw new IllegalJumpZeroException(index, opCode)

      case ((JumpNotZero(Literal(Zero), _) | JumpNotZero(_, Literal(One))), index) +: rest =>
        loop(rest, sourceCode ++ withLabel(index)(None))

      case (opCode @ JumpNotZero(test, Literal(offset)), index) +: rest =>
        loop(rest, sourceCode ++ withLabel(index) {
          val destination = index + offset.toInt
          val goto =
            if (destination > lastInstruction) Exit
            else labels.get(destination) match {
              case None    => throw new NoLabelForOffsetException(index, opCode, labels)
              case Some(l) => Goto(l)
            }
          test match {
            case Literal(_)      => goto.some
            case r @ Register(_) =>
              If(
                Conditional(Identity(r), NotEquals, Identity(Literal(Zero))),
                Vector(goto),
                Vector()).some
          }
        })
    }

    loop(assembly.zipWithIndex, Vector.empty)
  }

  def optimize(program: Vector[AST]): Try[Vector[AST]] = Try {
    def extractDoWhiles(remaining: Vector[AST]): Eval[Vector[AST]] = remaining match {
      case Vector() => Eval.now(Vector.empty)

      case lead :+ (elem @ Goto(label)) =>
        lead.indexOf(label) match {
          case -1 => Eval.defer(extractDoWhiles(lead).map(_ :+ elem))
          case i  =>
            val (rest, block) = lead.splitAt(i + 1)
            Eval.defer {
              for {
                doWhile <- Eval.defer(extractDoWhiles(block).map(DoWhile(True, _)))
                leadWithDoWhiles <- Eval.defer(extractDoWhiles(rest))
              } yield leadWithDoWhiles :+ doWhile
            }
        }

      case lead :+ (elem @ If(test, Vector(Goto(label)), Vector())) =>
        lead.indexOf(label) match {
          case -1 => Eval.defer(extractDoWhiles(lead).map(_ :+ elem))
          case i  =>
            val (rest, block) = lead.splitAt(i + 1)
            Eval.defer {
              for {
                doWhile <- Eval.defer(extractDoWhiles(block).map(DoWhile(test, _)))
                leadWithDoWhiles <- Eval.defer(extractDoWhiles(rest))
              } yield leadWithDoWhiles :+ doWhile
            }
        }

      case lead :+ (elem @ If(_, ifTrue, ifFalse)) =>
        for {
          ifTrueWithDoWhiles <- Eval.defer(extractDoWhiles(ifTrue))
          ifFalseWithDoWhiles <- Eval.defer(extractDoWhiles(ifFalse))
          leadWithDoWhiles <- Eval.defer(extractDoWhiles(lead))
        } yield
          leadWithDoWhiles :+ elem.copy(
            ifTrue = ifTrueWithDoWhiles,
            ifFalse = ifFalseWithDoWhiles
          )

      case lead :+ elem => Eval.defer(extractDoWhiles(lead).map(_ :+ elem))
    }

    def gatherIfBodies(remaining: Vector[AST]): Eval[Vector[AST]] = remaining match {
      case Vector() => Eval.now(Vector.empty)

      case (elem @ If(test, Vector(Goto(label)), Vector())) +: tail =>
        tail.indexOf(label) match {
          case -1 => Eval.defer(gatherIfBodies(tail).map(elem +: _))
          case i  =>
            val (block, rest) = tail.splitAt(i)
            Eval.defer {
              for {
                ifExp <- Eval.defer(gatherIfBodies(block).map(If(test, Vector.empty, _)))
                tailAST <- Eval.defer(gatherIfBodies(rest))
                recursed <- Eval.defer(gatherIfBodies(ifExp +: tailAST))
              } yield recursed
            }
        }

      case (elem @ If(test, Vector(), ifFalse :+ Goto(label))) +: tail =>
        tail.indexOf(label) match {
          case -1 => Eval.defer(gatherIfBodies(tail).map(elem +: _))
          case i  =>
            val (block, rest) = tail.splitAt(i)
            Eval.defer {
              for {
                ifExp <- Eval.defer(gatherIfBodies(block).map(If(test, _, ifFalse)))
                tailAST <- Eval.defer(gatherIfBodies(rest))
                recursed <- Eval.defer(gatherIfBodies(ifExp +: tailAST))
              } yield recursed
            }
        }

      case DoWhile(test, body) +: tail =>
        for {
          bodyWithGatheredIfs <- Eval.defer(gatherIfBodies(body))
          tailAST <- Eval.defer(gatherIfBodies(tail))
        } yield DoWhile(test, bodyWithGatheredIfs) +: tailAST

      case elem +: tail => Eval.defer(gatherIfBodies(tail).map(elem +: _))
    }

    def normalizeIfConditionals(remaining: Vector[AST]): Eval[Vector[AST]] = remaining match {
      case Vector() => Eval.now(Vector.empty)

      case If(Conditional(lhs, NotEquals, rhs), Vector(), ifFalse) +: tail =>
        for {
          normalizedBody <- Eval.defer(normalizeIfConditionals(ifFalse))
          normalizedTail <- Eval.defer(normalizeIfConditionals(tail))
        } yield If(Conditional(lhs, Equals, rhs), normalizedBody, Vector.empty) +: normalizedTail

      case DoWhile(test, body) +: tail =>
        for {
          normalizedBody <- Eval.defer(normalizeIfConditionals(body))
          tailAST <- Eval.defer(normalizeIfConditionals(tail))
        } yield DoWhile(test, normalizedBody) +: tailAST

      case elem +: tail => Eval.defer(normalizeIfConditionals(tail).map(elem +: _))
    }

    def stripUnreferencedLabels(input: Vector[AST]): Eval[Vector[AST]] = {
      def contains(label: Label, source: Vector[AST] = input): Eval[Boolean] =
        source.foldLeftM(false) {
          case (true, _)                   => Eval.True
          case (_, Goto(`label`))          => Eval.True
          case (_, If(_, ifTrue, ifFalse)) =>
            Eval.defer {
              for {
                containedInTrueBlock <- contains(label, ifTrue)
                containedInFalseBlock <- contains(label, ifFalse)
              } yield containedInTrueBlock || containedInFalseBlock
            }
          case (_, DoWhile(_, block))      => Eval.defer(contains(label, block))
          case _                           => Eval.False
        }

      def loop(remaining: Vector[AST]): Eval[Vector[AST]] = remaining match {
        case Vector() => Eval.now(Vector.empty)

        case (label @ Label(_)) +: tail =>
          Eval.defer {
            contains(label).flatMap {
              case true  => loop(tail).map(label +: _)
              case false => loop(tail)
            }
          }

        case If(test, ifTrue, ifFalse) +: tail =>
          for {
            strippedIfTrue <- Eval.defer(loop(ifTrue))
            strippedIfFalse <- Eval.defer(loop(ifFalse))
            strippedTail <- Eval.defer(loop(tail))
          } yield If(test, strippedIfTrue, strippedIfFalse) +: strippedTail

        case DoWhile(test, body) +: tail =>
          for {
            strippedBody <- Eval.defer(loop(body))
            strippedTail <- Eval.defer(loop(tail))
          } yield DoWhile(test, strippedBody) +: strippedTail

        case elem +: tail => Eval.defer(loop(tail).map(elem +: _))
      }

      loop(input)
    }

    /*def collapseAssignments(input: Vector[AST]): Eval[Vector[AST]] = {
      def increment(delta: Int)(v: Either[Int, Int]): Either[Int, Int] = v match {
        case Right(c) => Right(c + delta)
        case Left(c)  => Left(c + delta)
      }

      /**
        * Right indicates next assignment has not yet been encountered
        */
      def readsBeforeNextAssignment(register: Register, source: Vector[AST],
                                    trace: Boolean = false): Eval[Either[Int, Int]] =
        source match {
          case Vector()     =>
            if (trace) {
              println("Vector() => 0r")
            }
            Eval.now(0.asRight)
          case head +: tail =>
            val printTrace: Either[Int, Int] => Either[Int, Int] =
              if (!trace) identity
              else { i =>
                println(s"${head.show} -> $i")
                i
              }
            head match {
              case Assignment(`register`, rhs) => Eval.defer {
                readsBeforeNextAssignment(register, Vector(rhs)).map {
                  _.merge.asLeft
                }.map(printTrace)
              }

              case Identity(`register`)               => Eval.defer {
                readsBeforeNextAssignment(register, tail).map(increment(1)).map(printTrace)
              }
              case (Identity(_) | Goto(_) | Label(_)) => Eval.defer {
                readsBeforeNextAssignment(register, tail).map(printTrace)
              }

              case Addition(lhs, rhs)       => Eval.defer {
                readsBeforeNextAssignment(register, Vector(lhs, rhs)).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }
              case Subtraction(lhs, rhs)    => Eval.defer {
                readsBeforeNextAssignment(register, Vector(lhs, rhs)).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }
              case Multiplication(lhs, rhs) => Eval.defer {
                readsBeforeNextAssignment(register, Vector(lhs, rhs)).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }
              case Modulus(lhs, rhs)        => Eval.defer {
                readsBeforeNextAssignment(register, Vector(lhs, rhs)).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }

              case Assignment(_, rhs)        => Eval.defer {
                readsBeforeNextAssignment(register, Vector(rhs)).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }
              case If(test, ifTrue, ifFalse) => Eval.defer {
                readsBeforeNextAssignment(register, test +: (ifTrue ++ ifFalse)).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }
              case DoWhile(test, body)       => Eval.defer {
                readsBeforeNextAssignment(register, body :+ test).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }

              case Exit => Eval.defer(readsBeforeNextAssignment(register, tail).map(printTrace))

              case Conditional(lhs, _, rhs) => Eval.defer {
                readsBeforeNextAssignment(register, Vector(lhs, rhs)).flatMap {
                  case Right(c)    => readsBeforeNextAssignment(register, tail).map(increment(c))
                  case l @ Left(_) => Eval.now(l)
                }.map(printTrace)
              }
            }
        }

      def exactlyOneReadBeforeNextAssignment(register: Register, source: Vector[AST]): Eval[Boolean] =
        Eval.defer {
          readsBeforeNextAssignment(register, source, register.name == "b").map(_.merge == 1)
        }

      def processConditional(conditional: Conditional,
                             replacements: Map[Register, Expression]): Eval[(Conditional, Map[Register, Expression])] =
        Eval.defer {
          process(conditional.lhs, replacements).flatMap {
            case (updatedLhs, updatedReplacements0) =>
              process(conditional.rhs, updatedReplacements0).map {
                case (updatedRhs, updatedReplacements1) =>
                  (conditional.copy(lhs = updatedLhs, rhs = updatedRhs), updatedReplacements1)
              }
          }
        }

      def process(expression: Expression,
                  replacements: Map[Register, Expression]): Eval[(Expression, Map[Register, Expression])] =
        expression match {
          case Identity(Literal(_)) => Eval.now((expression, replacements))

          case Identity(r @ Register(_)) => Eval.now {
            replacements.get(r) match {
              case None      => (expression, replacements)
              case Some(exp) => (exp, replacements - r)
            }
          }

          case Addition(lhs, rhs) => Eval.defer {
            process(lhs, replacements).flatMap {
              case (updatedLhs, updatedReplacements0) =>
                process(rhs, updatedReplacements0).map {
                  case (updatedRhs, updatedReplacements1) =>
                    (Addition.apply(updatedLhs, updatedRhs), updatedReplacements1)
                }
            }
          }

          case Subtraction(lhs, rhs) => Eval.defer {
            process(lhs, replacements).flatMap {
              case (updatedLhs, updatedReplacements0) =>
                process(rhs, updatedReplacements0).map {
                  case (updatedRhs, updatedReplacements1) =>
                    (Subtraction.apply(updatedLhs, updatedRhs), updatedReplacements1)
                }
            }
          }

          case Multiplication(lhs, rhs) => Eval.defer {
            process(lhs, replacements).flatMap {
              case (updatedLhs, updatedReplacements0) =>
                process(rhs, updatedReplacements0).map {
                  case (updatedRhs, updatedReplacements1) =>
                    (Multiplication.apply(updatedLhs, updatedRhs), updatedReplacements1)
                }
            }
          }

          case Modulus(lhs, rhs) => Eval.defer {
            process(lhs, replacements).flatMap {
              case (updatedLhs, updatedReplacements0) =>
                process(rhs, updatedReplacements0).map {
                  case (updatedRhs, updatedReplacements1) =>
                    (Modulus.apply(updatedLhs, updatedRhs), updatedReplacements1)
                }
            }
          }

          case Assignment(r, rhs) => Eval.defer {
            process(rhs, replacements).map {
              case (updatedRhs, updatedReplacements) => (Assignment(r, updatedRhs), updatedReplacements)
            }
          }

          case If(test, ifTrue, ifFalse) => Eval.defer {
            processConditional(test, replacements).flatMap { case (updatedTest, updatedReplacements0) =>
              loop(ifTrue, Map.empty).flatMap { case (updatedIfTrue, _) =>
                loop(ifFalse, Map.empty).map { case (updatedIfFalse, _) =>
                  (If(updatedTest, updatedIfTrue, updatedIfFalse), updatedReplacements0)
                }
              }
            }
          }

          case DoWhile(test, body) => Eval.defer {
            loop(body, Map.empty).flatMap { case (updatedBody, bodyReplacements) =>
              processConditional(test, bodyReplacements).flatMap { case (updatedTest0, _) =>
                processConditional(updatedTest0, replacements).map { case (updatedTest1, updatedReplacements) =>
                  (DoWhile(updatedTest1, updatedBody), updatedReplacements)
                }
              }
            }
          }

          case Exit => Eval.now((Exit, replacements))

          case c @ Conditional(_, _, _) => Eval.defer(processConditional(c, replacements))
        }

      def loop(remaining: Vector[AST],
               replacements: Map[Register, Expression]): Eval[(Vector[AST], Map[Register, Expression])] =
        remaining match {
          case Vector() => Eval.now((Vector.empty, replacements))

          case Assignment(r, rhs) +: tail if exactlyOneReadBeforeNextAssignment(r, tail).value =>
            Eval.defer {
              process(rhs, replacements).flatMap {
                case (updatedRhs, updatedReplacements) =>
                  loop(tail, updatedReplacements + (r -> updatedRhs))
              }
            }

          case (elem: Expression) +: tail => Eval.defer {
            process(elem, replacements).flatMap {
              case (updatedElem, updatedReplacements0) =>
                loop(tail, updatedReplacements0).map {
                  case (updatedTail, updatedReplacements1) => (updatedElem +: updatedTail, updatedReplacements1)
                }
            }
          }

          case (elem @ (Goto(_) | Label(_))) +: tail => Eval.defer {
            loop(tail, replacements).map {
              case (updatedTail, updatedReplacements1) => (elem +: updatedTail, updatedReplacements1)
            }
          }
        }

      def metaLoop(program: Vector[AST]): Eval[Vector[AST]] =
        Eval.defer {
          loop(program, Map.empty).map(_._1).flatMap { output =>
            if (program == output) Eval.now(output)
            else metaLoop(output)
          }
        }

      metaLoop(input)
    }*/

    Seq(
      extractDoWhiles _,
      gatherIfBodies _,
      normalizeIfConditionals _,
      stripUnreferencedLabels _
      //collapseAssignments _
    ).foldLeft(Eval.now(program))(_ flatMap _).value
  }

  def execute(program: Vector[AST]): Try[Memory] = {
    def eval(ast: AST)(implicit memory: Memory): Eval[(Literal, Memory)] = ast match {
      case Label(_) | Goto(_)        => throw new IllegalArgumentException(s"Unoptimized meta: ${ast.show}")

      case Identity(l @ Literal(_))  => Eval.now((l, memory))
      case Identity(r @ Register(_)) => Eval.later((r.resolve, memory))

      case Addition(lhs, rhs)        => Eval.defer {
        eval(lhs).flatMap { case (lhLiteral, _) =>
          eval(rhs).map { case (rhLiteral, _) =>
            (lhLiteral + rhLiteral, memory)
          }
        }
      }

      case Subtraction(lhs, rhs)     => Eval.defer {
        eval(lhs).flatMap { case (lhLiteral, _) =>
          eval(rhs).map { case (rhLiteral, _) =>
            (lhLiteral - rhLiteral, memory)
          }
        }
      }
      case Multiplication(lhs, rhs)  => Eval.defer {
        eval(lhs).flatMap { case (lhLiteral, _) =>
          eval(rhs).map { case (rhLiteral, _) =>
            (lhLiteral * rhLiteral, memory)
          }
        }
      }
      case Modulus(lhs, rhs)         => Eval.defer {
        eval(lhs).flatMap { case (lhLiteral, _) =>
          eval(rhs).map { case (rhLiteral, _) =>
            (lhLiteral % rhLiteral, memory)
          }
        }
      }

      case Assignment(r, rhs) => Eval.defer {
        eval(rhs).map { case (rhsValue, _) =>
          (rhsValue, memory.put(r, rhsValue))
        }
      }

      case If(test, ifTrue, ifFalse) => Eval.defer {
        eval(test).flatMap {
          case (Literal(One), _) => loop(ifTrue, memory)
          case _ => loop(ifFalse, memory)
        }
      }

      case block @ DoWhile(test, body) => Eval.defer {
        loop(body, memory).flatMap {
          case (bodyResult, updatedMemory) =>
            eval(test)(updatedMemory).flatMap {
              case (Literal(One), _) => eval(block)(updatedMemory)
              case _ => Eval.now((bodyResult, updatedMemory))
            }
        }
      }

      case Exit => Eval.now((Literal(Zero), memory))

      case Conditional(lhs, op, rhs) => Eval.defer {
        eval(lhs).flatMap { case (lhLiteral, _) =>
          eval(rhs).map { case (rhLiteral, _) =>
            (Literal(if (lhLiteral == rhLiteral && op == Equals) One else Zero), memory)
          }
        }
      }
    }

    def loop(rest: Vector[AST], memory: Memory): Eval[(Literal, Memory)] = rest match {
      case Exit +: _ | Vector() => Eval.now((Literal(Zero), memory))
      case head +: Vector() => Eval.defer {
        eval(head)(memory).map {
          case (result, updatedMemory) => (result, updatedMemory)
        }
      }
      case head +: tail => Eval.defer {
        eval(head)(memory).flatMap {
          case (_, updatedMemory) =>
            loop(tail, updatedMemory)
        }
      }
    }

    Try(loop(program, Memory.empty).value._2)
  }

  def run(input: String): Try[Vector[AST]] =
    for {
      program <- parse(input.trim)
      decompiled <- decompile(program)
      optimized <- optimize(decompiled)
    } yield optimized
}
