package com.peschke.advent_of_code
package day7

import scala.util.Try
import cats.data.NonEmptyList

object Part1 {
  def buildTree(infoSeq: Seq[ProgramInfo]): Try[Program] = {
    def loop(remaining: Set[ProgramInfo], accum: Map[Name,Program]): List[Program] =
      if (remaining.isEmpty) accum.values.toList
      else remaining.find(_.supporting.forall(accum.contains)) match {
        case None => throw new NoSuchElementException(
          "Unable to find a program with resolved dependencies")
        case Some(info) =>
          val program =
            info.supporting.flatMap(accum.get) match {
              case Nil => Program.BalanceHelper(info.name, info.weight)
              case n :: ns => Program.DiscHolder(
                info.name,
                info.weight,
                NonEmptyList.of(n, ns:_*))
            }

          val newAccum = program match {
            case Program.BalanceHelper(_, _) => accum
            case Program.DiscHolder(_, _, h) => accum -- h.map(_.name).toList
          }

          loop(
            remaining - info,
            newAccum + (info.name -> program))
      }

    Try {
      loop(infoSeq.toSet, Map.empty) match {
        case Nil => throw new NoSuchElementException("Nothing extracted")
        case p :: Nil => p
        case ps => throw new NoSuchElementException(s"Unable to extract a single program: $ps")
      }
    }
  }

  def whoIsOnBottom(input: String): Try[Name] =
    RecursiveCircus
      .parse(input)
      .flatMap(buildTree)
      .map(_.name)
      .mapError(RecursiveCircus, input)
}
