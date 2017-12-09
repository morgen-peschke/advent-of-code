package com.peschke.advent_of_code.day3

class SpiralMemoryFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"Day3SpiralMemory failed on input:\n$input", cause)

case class Steps(n: Int) {
  override def toString: String = s"$n steps"

  def + (other: Steps): Steps = Steps(this.n + other.n)
  def + (delta: Int): Steps = Steps(this.n + delta)

  def - (other: Steps): Steps = Steps(this.n - other.n)
  def - (delta: Int): Steps = Steps(this.n - delta)

  def unary_- : Steps = Steps(-this.n)

  def abs: Steps = Steps(n.abs)

  def render(numberWidth: Int): String = s"${n.toString.padTo(numberWidth, ' ')} steps"
}

object Steps {
  val zero: Steps = Steps(0)

  object syntax {
    implicit class IntSyntax(val i: Int) extends AnyVal {
      def steps: Steps = Steps(i)
    }
  }

  implicit val ordering: Ordering[Steps] = Ordering.by(_.n)
}

case class Offset(vertical: Steps = Steps.zero, horizontal: Steps = Steps.zero) {
  def - (other: Offset): Offset =
    Offset(
      vertical = this.vertical - other.vertical,
      horizontal = this.horizontal - other.horizontal
    )

  def distance: Steps = vertical.abs + horizontal.abs

  def up: Offset = copy(vertical = vertical - 1)
  def down: Offset = copy(vertical = vertical + 1)
  def left: Offset = copy(horizontal = horizontal - 1)
  def right: Offset = copy(horizontal = horizontal + 1)

  def arrow: String = (vertical.n, horizontal.n) match {
    case (0, h) if h < 0 => "→"
    case (0, h) if h > 0 => "←"
    case (v, 0) if v < 0 => "↓"
    case (v, 0) if v > 0 => "↑"
    case (v, h) if v < 0 && h < 0 => "↘"
    case (v, h) if v < 0 && h > 0 => "↙"
    case (v, h) if v > 0 && h < 0 => "↗"
    case (v, h) if v > 0 && h > 0 => "↖"
    case (0, 0) => " "
    case _ => " "
  }

  def render(numberWidth: Int): String =
    Seq(
      "[",
      arrow,
      " ",
      horizontal.abs.render(numberWidth),
      ", ",
      vertical.abs.render(numberWidth),
      "]"
    ).mkString
}

object Offset {
  val zero: Offset = Offset(Steps.zero, Steps.zero)
}
