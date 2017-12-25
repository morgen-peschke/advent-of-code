package com.peschke.advent_of_code
package day12

import scala.util.Try

object Part1 {
  def parse(input: String): Try[Seq[Pipe]] = Try {
    input.split('\n')
      .toVector
      .map(Parser.parse)
      .flatMap {
        case (program, connections) => connections.map(Pipe(program, _))
      }
      .distinct
      .sorted
  }

  def findConnections(root: Program, pipes: Seq[Pipe]): Set[Program] = {
    def loop(p: Program, accum: Set[Program]): Set[Program] = {
      val connectedPrograms =
        pipes
          .collect {
            case Pipe(`p`, pOther) => pOther
            case Pipe(pOther, `p`) => pOther
          }
          .filterNot(accum.contains _)
      connectedPrograms.foldLeft(accum ++ connectedPrograms) {
        case (currAccum, pOther) => loop(pOther, currAccum)
      }
    }

    loop(root, Set(root))
  }

  def numberOfConnections(program: Program, input: String): Try[Int] =
    parse(input)
      .map(findConnections(program, _))
      .map(_.size)
      .mapError(new DigitalPlumberFailure(input, _))
}
