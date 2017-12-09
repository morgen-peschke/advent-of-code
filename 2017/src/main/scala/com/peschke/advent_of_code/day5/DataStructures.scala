package com.peschke.advent_of_code.day5

class TrampolineMazeFailure(input: String, cause: Throwable)
    extends IllegalArgumentException(s"Trampoline Maze failed on input:\n$input", cause)

case class Instructions(jumpOffsets: Vector[Int], index: Int = 0) {
  def render(columnWidth: Option[Int] = None, limitToContext: Option[Int] = None): String = {
    val indexed = jumpOffsets.zipWithIndex
    val toRender = limitToContext match {
      case None => indexed
      case Some(context) =>
        val window = (index - context.abs) to (index + context.abs)
        val truncated = window.flatMap(indexed.lift(_))
        if (truncated.isEmpty) indexed.takeRight(context) else truncated
    }
    val rendered =
      toRender.map {
        case (jumpOffset, jumpIndex) if jumpIndex == index => s"($jumpOffset)"
        case (jumpOffset, _) => s"$jumpOffset"
      }

    Seq(
      "[",
      columnWidth
        .fold(rendered)(w => rendered.map(_.reverse.padTo(w, ' ').reverse))
        .mkString(" "),
      "]").mkString
  }
}
