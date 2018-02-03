package com.peschke.advent_of_code
package day23

import cats.Show
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.show._
import com.peschke.advent_of_code.day23.Expression.Conditional.{Equals, NotEquals}
import com.peschke.advent_of_code.day23.Expression._
import com.peschke.advent_of_code.day23.Meta._

import scala.util.Try

/**
  * http://adventofcode.com/2017/day/23
  *
  * --- Day 23: Coprocessor Conflagration ---
  *
  * You decide to head directly to the CPU and fix the printer from
  * there. As you get close, you find an experimental coprocessor
  * doing so much work that the local programs are afraid it will halt
  * and catch fire. This would cause serious issues for the rest of
  * the computer, so you head in and see what you can do.
  *
  * The code it's running seems to be a variant of the kind you saw
  * recently on that tablet. The general functionality seems very
  * similar, but some of the instructions are different:
  *
  *  - set X Y sets register X to the value of Y.
  *  - sub X Y decreases register X by the value of Y.
  *  - mul X Y sets register X to the result of multiplying the value
  * contained in register X by the value of Y.
  *  - jnz X Y jumps with an offset of the value of Y, but only if the
  * value of X is not zero. (An offset of 2 skips the next
  * instruction, an offset of -1 jumps to the previous
  * instruction, and so on.)
  *
  * Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.
  *
  * The coprocessor is currently set to some kind of debug mode, which
  * allows for testing, but prevents it from doing any meaningful
  * work.
  *
  * If you run the program (your puzzle input), how many times is the
  * mul instruction invoked?
  *
  * --- Part Two ---
  *
  * Now, it's time to fix the problem.
  *
  * The debug mode switch is wired directly to register a. You flip
  * the switch, which makes register a now start at 1 when the program
  * is executed.
  *
  * Immediately, the coprocessor begins to overheat. Whoever wrote
  * this program obviously didn't choose a very efficient
  * implementation. You'll need to optimize the program if it has any
  * hope of completing before Santa needs that printer working.
  *
  * The coprocessor's ultimate goal is to determine the final value
  * left in register h once the program completes. Technically, if it
  * had that... it wouldn't even need to run the program.
  *
  * After setting register a to 1, if the program were to run to
  * completion, what value would be left in register h?
  */
object CoprocessorConflagration extends AdventOfCodeDay {
  override type P1 = Int
  override type P2 = String

  implicit val vectorSourceShow: Show[Vector[AST]] = unwrappedVectorShow[AST]

  override def runPart1(input: String): Try[P1] = Part1.debug(input)

  override def runPart2(input: String): Try[P2] =
    Part2.run(input).map { p =>
      val start = (65 * 100) + 100000
      val end = start + 17000
      val count =
        Iterator
          .from(start, 17)
          .takeWhile(_ <= end)
          .map(BigInt(_))
          .filterNot(_.isProbablePrime(10))
          .length.toString

      s"""$count
         |${p.show}
       """.stripMargin
    }

  private val raw: String =
    """set x 10
      |set y 15
      |mul x 2
      |mul y -3
      |set b x
      |sub b y
      |set c b
      |jnz a 3
      |set z 1
      |jnz 1 5
      |mul b 100
      |sub b -100000
      |set c b
      |sub c -17000
      |set f 1
      |set d 2
      |set e 2
      |set g d
      |mul g e
      |sub g b
      |jnz g 2
      |set f 0
      |sub e -1
      |set g e
      |sub g b
      |jnz g -8
      |sub d -1
      |set g d
      |sub g b
      |jnz g -13
      |jnz f 2
      |sub h -1
      |set g b
      |sub g c
      |jnz g 2
      |jnz 1 3
      |sub b -17
      |jnz 1 -23
    """.stripMargin

  private val parsed: Vector[OpCode] = Vector(
    Set(Register("x"), Literal(10)),
    Set(Register("y"), Literal(15)),
    Multiply(Register("x"), Literal(2)),
    Multiply(Register("y"), Literal(-3)),
    Set(Register("b"), Register("x")),
    Sub(Register("b"), Register("y")),
    Set(Register("c"), Register("b")),
    JumpNotZero(Register("a"), Literal(3)),
    Set(Register("z"), Literal(1)),
    JumpNotZero(Literal(1), Literal(5)),
    Multiply(Register("b"), Literal(100)),
    Sub(Register("b"), Literal(-100000)),
    Set(Register("c"), Register("b")),
    Sub(Register("c"), Literal(-17000)),
    Set(Register("f"), Literal(1)),
    Set(Register("d"), Literal(2)),
    Set(Register("e"), Literal(2)),
    Set(Register("g"), Register("d")),
    Multiply(Register("g"), Register("e")),
    Sub(Register("g"), Register("b")),
    JumpNotZero(Register("g"), Literal(2)),
    Set(Register("f"), Literal(0)),
    Sub(Register("e"), Literal(-1)),
    Set(Register("g"), Register("e")),
    Sub(Register("g"), Register("b")),
    JumpNotZero(Register("g"), Literal(-8)),
    Sub(Register("d"), Literal(-1)),
    Set(Register("g"), Register("d")),
    Sub(Register("g"), Register("b")),
    JumpNotZero(Register("g"), Literal(-13)),
    JumpNotZero(Register("f"), Literal(2)),
    Sub(Register("h"), Literal(-1)),
    Set(Register("g"), Register("b")),
    Sub(Register("g"), Register("c")),
    JumpNotZero(Register("g"), Literal(2)),
    JumpNotZero(Literal(1), Literal(3)),
    Sub(Register("b"), Literal(-17)),
    JumpNotZero(Literal(1), Literal(-23)))

  private val lightlyOptimizedSource: Vector[AST] = Vector(
    Assignment(Register("x"), Identity(Literal(10))),
    Assignment(Register("y"), Identity(Literal(15))),
    Assignment(Register("x"), Multiplication(Identity(Register("x")), Identity(Literal(2)))),
    Assignment(Register("y"), Multiplication(Identity(Register("y")), Identity(Literal(-3)))),
    Assignment(Register("b"), Subtraction(Identity(Register("x")), Identity(Register("y")))),
    Assignment(Register("c"), Identity(Register("b"))),
    If(Conditional(Identity(Register("a")), NotEquals, Identity(Literal(0))), Vector(Goto(Label("L1"))), Vector.empty),
    Assignment(Register("z"), Identity(Literal(1))),
    Goto(Label("L2")),
    Label("L1"),
    Assignment(
      Register("b"),
      Addition(
        Multiplication(
          Identity(Register("b")),
          Identity(Literal(100))),
        Identity(Literal(100000)))),
    Assignment(Register("c"), Addition(Identity(Register("b")), Identity(Literal(17000)))),
    Label("L2"),
    Assignment(Register("f"), Identity(Literal(1))),
    Assignment(Register("d"), Identity(Literal(2))),
    Label("L5"),
    Assignment(Register("e"), Identity(Literal(2))),
    Label("L4"),
    Assignment(
      Register("g"),
      Subtraction(
        Multiplication(
          Identity(Register("d")),
          Identity(Register("e"))),
        Identity(Register("b")))),
    If(Conditional(Identity(Register("g")), NotEquals, Identity(Literal(0))), Vector(Goto(Label("L3"))), Vector.empty),
    Assignment(Register("f"), Identity(Literal(0))),
    Label("L3"),
    Assignment(Register("e"), Addition(Identity(Register("e")), Identity(Literal(1)))),
    Assignment(Register("g"), Subtraction(Identity(Register("e")), Identity(Register("b")))),
    If(Conditional(Identity(Register("g")), NotEquals, Identity(Literal(0))), Vector(Goto(Label("L4"))), Vector.empty),
    Assignment(Register("d"), Addition(Identity(Register("d")), Identity(Literal(1)))),
    Assignment(Register("g"), Subtraction(Identity(Register("d")), Identity(Register("b")))),
    If(Conditional(Identity(Register("g")), NotEquals, Identity(Literal(0))), Vector(Goto(Label("L5"))), Vector.empty),
    If(Conditional(Identity(Register("f")), NotEquals, Identity(Literal(0))), Vector(Goto(Label("L6"))), Vector.empty),
    Assignment(Register("h"), Addition(Identity(Register("h")), Identity(Literal(1)))),
    Label("L6"),
    Assignment(Register("g"), Subtraction(Identity(Register("b")), Identity(Register("c")))),
    If(Conditional(Identity(Register("g")), NotEquals, Identity(Literal(0))), Vector(Goto(Label("L7"))), Vector.empty),
    Exit,
    Label("L7"),
    Assignment(Register("b"), Addition(Identity(Register("b")), Identity(Literal(17)))),
    Goto(Label("L2"))
  )

  private val optimizedSource: Vector[AST] = Vector(
    Assignment(Register("x"), Identity(Literal(10))),
    Assignment(Register("y"), Identity(Literal(15))),
    Assignment(Register("x"), Multiplication(Identity(Register("x")), Identity(Literal(2)))),
    Assignment(Register("y"), Multiplication(Identity(Register("y")), Identity(Literal(-3)))),
    Assignment(Register("b"), Subtraction(Identity(Register("x")), Identity(Register("y")))),
    Assignment(Register("c"), Identity(Register("b"))),
    If(
      Conditional(Identity(Register("a")), NotEquals, Identity(Literal(0))),
      Vector(
        Assignment(
          Register("b"),
          Addition(
            Multiplication(
              Identity(Register("b")),
              Identity(Literal(100))),
            Identity(Literal(100000)))),
        Assignment(Register("c"), Addition(Identity(Register("b")), Identity(Literal(17000))))),
      Vector(
        Assignment(Register("z"), Identity(Literal(1))))),
    DoWhile(
      Conditional(Identity(Literal(BigInt(1))), Equals, Identity(Literal(BigInt(1)))),
      Vector(
        Assignment(Register("f"), Identity(Literal(1))),
        Assignment(Register("d"), Identity(Literal(2))),
        DoWhile(
          Conditional(Identity(Register("g")), NotEquals, Identity(Literal(0))),
          Vector(
            Assignment(Register("e"), Identity(Literal(2))),
            DoWhile(
              Conditional(Identity(Register("g")), NotEquals, Identity(Literal(0))),
              Vector(
                Assignment(
                  Register("g"),
                  Subtraction(
                    Multiplication(
                      Identity(Register("d")),
                      Identity(Register("e"))),
                    Identity(Register("b")))),
                If(
                  Conditional(Identity(Register("g")), Equals, Identity(Literal(0))),
                  Vector(Assignment(Register("f"), Identity(Literal(0)))),
                  Vector.empty),
                Assignment(Register("e"), Addition(Identity(Register("e")), Identity(Literal(1)))),
                Assignment(Register("g"), Subtraction(Identity(Register("e")), Identity(Register("b"))))
              )),
            Assignment(Register("d"), Addition(Identity(Register("d")), Identity(Literal(1)))),
            Assignment(Register("g"), Subtraction(Identity(Register("d")), Identity(Register("b"))))
          )),
        If(
          Conditional(Identity(Register("f")), Equals, Identity(Literal(0))),
          Vector(Assignment(Register("h"), Addition(Identity(Register("h")), Identity(Literal(1))))),
          Vector.empty),
        Assignment(Register("g"), Subtraction(Identity(Register("b")), Identity(Register("c")))),
        If(
          Conditional(Identity(Register("g")), Equals, Identity(Literal(0))),
          Vector(Exit),
          Vector.empty),
        Assignment(Register("b"), Addition(Identity(Register("b")), Identity(Literal(17))))
      )))

  override def verifyPart1Samples(): Unit = {
    cats.Show[OpCode].show(parsed.head)
    println(Part1.parse(raw).asResult(parsed).show)
  }

  override def verifyPart2Samples(): Unit = {
    println("---- pretty printing ----")
    println(
      Try(parsed.map(_.show).mkString("\n"))
        .asResult(raw.trim)
        .show)

    println("---- source code optimization ----")
    println(Part2.decompile(parsed).asResult(lightlyOptimizedSource).show)
    println(Part2.optimize(lightlyOptimizedSource).asResult(optimizedSource).show)
  }
}
