package com.peschke.advent_of_code
package day13

import scala.util.Try

object Part1 {
  def parse(input: String): Try[Firewall] = Try {
    val layersWithScanners =
      input
        .split('\n')
        .map { line =>
          line.split(':').map(_.trim) match {
            case Array(layerStr, rangeStr) =>
              layerStr.toInt -> Scanner(rangeStr.toInt, 0, Down)
            case _ =>
              throw new IllegalArgumentException(s"Line does not conform to expected input:\n$line")
          }
        }
        .toMap

    Firewall(
      (0 to layersWithScanners.keySet.max)
        .map { layerId =>
          Layer(false, layersWithScanners.get(layerId))
        }
        .toVector
    )
  }

  implicit class ScannerOps(val sc: Scanner) extends AnyVal {
    def maxLocation: Int = sc.range - 1

    def tick: Scanner =
      if (sc.range == 1)
        sc
      else {
        val newDirection =
          if (sc.location == 0 && sc.direction == Up) Down
          else if (sc.location == maxLocation && sc.direction == Down) Up
          else sc.direction
        val newLocation = newDirection match {
          case Up   => sc.location - 1
          case Down => sc.location + 1
        }
        sc.copy(
          location = newLocation,
          direction = newDirection)
      }
  }

  implicit class FirewallOps(val fw: Firewall) extends AnyVal {
    def noPacketInLayers: Boolean = !fw.layers.exists(_.packetInLayer)

    def tick: (Firewall, Vector[Caught]) =
      fw.layers.zipWithIndex.foldLeft((Vector.empty[Layer], Vector.empty[Caught], noPacketInLayers)) {
        case (
          (updatedLayers, timesCaught, packetEntering),
          (Layer(packetInLayer, None), _)
        ) =>
          (
            updatedLayers :+ Layer(packetEntering, None),
            timesCaught,
            packetInLayer
          )
        case (
          (updatedLayers, timesCaught, packetEntering),
          (Layer(packetInLayer, Some(scanner)), index)
        ) =>
          (
            updatedLayers :+ Layer(packetEntering, Some(scanner.tick)),
            if (packetEntering && scanner.location == 0) timesCaught :+ Caught(index, scanner)
            else timesCaught,
            packetInLayer
          )
      } match {
        case (layers, timesCaught, _) => (Firewall(layers), timesCaught)
      }
  }

  def traverseFirewall(input: String): Try[Severity] =
    parse(input).map { initial =>
      def loop(prev: Firewall, accum: Vector[Caught]): Severity = {
        val (next, caught) = prev.tick
        val totalCaught = accum ++ caught
        if (next.noPacketInLayers)
          Severity(totalCaught
            .map {
              case Caught(depth, Scanner(range, _, _)) => depth * range
            }
            .sum)
        else loop(next, totalCaught)
      }
      loop(initial, Vector.empty)
    }
}
