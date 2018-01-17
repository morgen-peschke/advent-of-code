package com.peschke.advent_of_code
package day20

object Part2 {
  import Part1.ParticleOps

  def simulate(initial: Vector[Particle], iterations: Int): Vector[(Int, Particle)] =
    (0 until iterations).foldLeft(initial.zipWithIndex.map(_.swap)) { (particles, _) =>
      particles
        .map {
          case (id, particle) => id -> particle.step
        }
        .groupBy(_._2.point)
        .toVector
        .collect {
          case (_, Vector(idAndParticle)) => idAndParticle
        }
        .sortBy(_._1)
    }
}
