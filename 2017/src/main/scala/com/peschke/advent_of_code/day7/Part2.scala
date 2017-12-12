package com.peschke.advent_of_code
package day7

import cats.syntax.option._

import scala.util.Try

object Part2 {
  def calculateWeights(program: Program, memo: Map[Name, Int] = Map.empty): Map[Name, Int] =
    program match {
      case Program.BalanceHelper(name, weight) => memo + (name -> weight)
      case Program.DiscHolder(name, weight, holding) =>
        val (supportingWeight, newMemo) = holding.foldLeft((0, memo)) {
          case ((weightAccum, memoAccum), held) =>
            memoAccum.get(held.name) match {
              case Some(w) => (weightAccum + w, memoAccum)
              case None =>
                val updatedMemo = calculateWeights(held, memoAccum)
                val w = updatedMemo.get(held.name).getOrElse {
                  throw new NoSuchElementException(
                    s"Returned weights did not include ${held.name}")
                }
                (weightAccum + w, updatedMemo)
            }
        }
        val totalWeight = supportingWeight + weight
        newMemo + (name -> totalWeight)
    }

  def findUnbalanced(root: Program): Option[UnbalancedProgram] = {
    val weights = calculateWeights(root)
    def lookupWeight(p: Program): Int = weights.getOrElse(p.name,
      throw new NoSuchElementException(s"No precalculated weight for ${p.name}"))

    def loop(program: Program): List[UnbalancedProgram] = program match {
      case Program.BalanceHelper(_, _) => Nil
      case Program.DiscHolder(_, _, holding) =>
        val weights = holding.map(lookupWeight).groupBy(identity).map {
          case (w, l) => w -> l.size
        }
        if (weights.size == 1) Nil
        else {
          val probablyCorrectWeight =
            weights.toSeq.sortBy(-_._2).headOption.map(_._1).getOrElse {
              throw new NoSuchElementException(s"No mode for $weights")
            }
          val probablyHaveIncorrectWeight =
            holding.toList.flatMap { p =>
              Option(p -> (probablyCorrectWeight - lookupWeight(p))).filterNot(_._2 == 0)
            }

          probablyHaveIncorrectWeight match {
            case Nil => throw new NoSuchElementException(
              s"$program is unbalanced, but all supported programs have the same weight: " +
                s"$probablyCorrectWeight")

            case unbalancedHeld =>
              unbalancedHeld.flatMap {
                case (held, delta) =>
                  loop(held) match {
                    case Nil => List(UnbalancedProgram(held.name, held.weight, delta))
                    case ls => ls
                  }
              }
          }
        }
    }

    loop(root) match {
      case Nil => None
      case p :: Nil => p.some
      case ps =>
        throw new NoSuchElementException(s"Unable to locate a single unbalanced node: $ps")
    }
  }

  def findCorrectWeightForUnbalanced(input: String): Try[Int] =
    RecursiveCircus.parse(input)
      .flatMap(Part1.buildTree _)
      .flatMap { tree =>
        Try {
          val unbalanced = findUnbalanced(tree).getOrElse {
            throw new NoSuchElementException("Unable to locate unbalanced node")
          }
          unbalanced.weight + unbalanced.delta
        }
      }.mapError(new RecursiveCircusFailure(input, _))
}
