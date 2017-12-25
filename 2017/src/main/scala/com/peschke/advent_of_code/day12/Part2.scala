package com.peschke.advent_of_code
package day12

import scala.util.Try

object Part2 {
  def separateGroups(pipes: Seq[Pipe]): Set[Set[Program]] = {
    def loop(remaining: Set[Pipe], accum: Set[Set[Program]]): Set[Set[Program]] =
      remaining.headOption.map(_.p0) match {
        case None => accum
        case Some(program) =>
          val connections = Part1.findConnections(program, remaining.toSeq)
          loop(
            remaining.filterNot(pipe => connections(pipe.p0) || connections(pipe.p1)),
            accum + connections
          )
      }

    loop(pipes.toSet, Set.empty)
  }

  def countGroups(input: String): Try[Int] =
    Part1.parse(input)
      .map(separateGroups)
      .map(_.size)
      .mapError(new DigitalPlumberFailure(input, _))
}
