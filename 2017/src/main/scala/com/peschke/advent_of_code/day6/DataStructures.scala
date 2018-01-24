package com.peschke.advent_of_code
package day6

import cats.Order
import cats.data.NonEmptyVector

import cats.syntax.reducible._
import cats.syntax.option._

import cats.instances.int._

object Memory {
  case class Bank(blocks: Int) {
      def increment: Memory.Bank = Memory.Bank(blocks + 1)
  }

  object Bank {
    val empty: Bank = Bank(0)

    implicit val ordering: Order[Bank] = Order.by(_.blocks)
  }

  case class Region(banks: NonEmptyVector[Bank]) {
    def zeroBankAt(index: Int): Option[Memory.Region] =
      banks.updated(index, Memory.Bank.empty).map { b =>
        copy(banks = b)
      }

    def incrementBankAt(index: Int): Option[Memory.Region] =
      for {
        oldValue <- banks.get(index)
        updated  <- banks.updated(index, oldValue.increment)
      } yield copy(banks = updated)

    def bankWithMostBlocks: Option[(Memory.Bank, Int)] = {
      val maxBank = banks.maximum
      banks.zipWithIndex.find {
        case (bank, _) => maxBank == bank
      }
    }

    def reallocate: Option[Memory.Region] = {
      def indexes(start: Int, length: Int): Iterator[Int] =
        Iterator.iterate(start) { current =>
          val next = current + 1
          if (next < length) next else 0
        }

      bankWithMostBlocks.flatMap {
        case (maxBank, indexOfMaxBank) =>
          zeroBankAt(indexOfMaxBank).flatMap { banksWithMaxZeroed =>
            indexes(indexOfMaxBank, banks.length)
              .drop(1)
              .take(maxBank.blocks)
              .foldLeft(banksWithMaxZeroed.some) {
                case (None, _) => None
                case (Some(r), index) => r.incrementBankAt(index)
              }
          }
      }
    }

    def render(columnWidth: Int): String =
      banks
        .map(_.blocks.toString.reverse.padTo(columnWidth, ' ').reverse)
        .toVector
        .mkString("Region[", " ", "]")
  }

  object Region {
    def containing(v1: Int, vs: Int*): Region =
      Region(NonEmptyVector.of(v1, vs:_*).map(Memory.Bank(_)))
  }
}
