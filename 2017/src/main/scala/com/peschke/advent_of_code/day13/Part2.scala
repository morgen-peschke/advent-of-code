package com.peschke.advent_of_code
package day13

import scala.util.Try

import Part1.ScannerOps

object Part2 {
  implicit class FirewallOps(val fw: Firewall) extends AnyVal {
    def noPacketInLayers: Boolean = !fw.layers.exists(_.packetInLayer)

    def delay: Firewall = tick(false)._1

    def tick(introducePacket: Boolean): (Firewall, Vector[Caught]) =
      fw.layers.zipWithIndex.foldLeft((Vector.empty[Layer], Vector.empty[Caught], introducePacket)) {
        case (
          (updatedLayers, timesCaught, packetEntering),
          (Layer(packetInLayer, scannerOpt), index)
        ) =>
          (
            updatedLayers :+ Layer(packetEntering, scannerOpt.map(_.tick)),
            scannerOpt.fold(timesCaught) { sc =>
              if (packetEntering && sc.location == 0) timesCaught :+ Caught(index, sc)
              else timesCaught
            },
            packetInLayer
          )
      } match {
        case (layers, timesCaught, _) => (Firewall(layers), timesCaught)
      }
  }

  def wouldBeCaught(firewall: Firewall): Boolean = {
    def loop(prev: Firewall): Boolean = prev.tick(false) match {
      case (next, Vector()) if next.noPacketInLayers => false
      case (next, Vector()) => loop(next)
      case _ => true
    }
    val (initial, caughtOnEntry) = firewall.tick(true)
    if (caughtOnEntry.isEmpty) loop(initial)
    else true
  }

  def traverseFirewallSafely(input: String): Try[Delay] =
    Part1.parse(input).map { firewall =>
      Delay {
        Iterator
          .iterate(firewall)(_.delay)
          .takeWhile(wouldBeCaught)
          .size
      }
    }
}
