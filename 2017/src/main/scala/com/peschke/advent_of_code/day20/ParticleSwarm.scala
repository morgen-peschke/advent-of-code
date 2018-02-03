package com.peschke.advent_of_code
package day20

import cats.{Eq, Show}

import scala.util.Try
import cats.syntax.show._
import cats.instances.tuple._
import cats.instances.vector._
import cats.instances.int._

/**
  * http://adventofcode.com/2017/day/20
  *
  * --- Day 20: Particle Swarm ---
  *
  * Suddenly, the GPU contacts you, asking for help. Someone has asked
  * it to simulate too many particles, and it won't be able to finish
  * them all in time to render the next frame at this rate.
  *
  * It transmits to you a buffer (your puzzle input) listing each
  * particle in order (starting with particle 0, then particle 1,
  * particle 2, and so on). For each particle, it provides the X, Y,
  * and Z coordinates for the particle's position (p), velocity (v),
  * and acceleration (a), each in the format <X,Y,Z>.
  *
  * Each tick, all particles are updated simultaneously. A particle's
  * properties are updated in the following order:
  *
  * - Increase the X velocity by the X acceleration.
  * - Increase the Y velocity by the Y acceleration.
  * - Increase the Z velocity by the Z acceleration.
  * - Increase the X position by the X velocity.
  * - Increase the Y position by the Y velocity.
  * - Increase the Z position by the Z velocity.
  *
  * Because of seemingly tenuous rationale involving z-buffering, the
  * GPU would like to know which particle will stay closest to
  * position <0,0,0> in the long term. Measure this using the
  * Manhattan distance, which in this situation is simply the sum of
  * the absolute values of a particle's X, Y, and Z position.
  *
  * For example, suppose you are only given two particles, both of
  * which stay entirely on the X-axis (for simplicity). Drawing the
  * current states of particles 0 and 1 (in that order) with an
  * adjacent a number line and diagram of current X positions (marked
  * in parenthesis), the following would take place:
  *
  * {{{
  * p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)
  *
  * p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)
  *
  * p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)
  *
  * p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
  * p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)
  * }}}
  *
  * At this point, particle 1 will never be closer to <0,0,0> than
  * particle 0, and so, in the long run, particle 0 will stay closest.
  *
  * Which particle will stay closest to position <0,0,0> in the long term?
  *
  * --- Part Two ---
  *
  * To simplify the problem further, the GPU would like to remove any
  * particles that collide. Particles collide if their positions ever
  * exactly match. Because particles are updated simultaneously, more
  * than two particles can collide at the same time and place. Once
  * particles collide, they are removed and cannot collide with
  * anything else after that tick.
  *
  * For example:
  * {{{
  * p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
  * p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  * p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
  * p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>
  *
  * p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
  * p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  * p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
  * p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>
  *
  * p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
  * p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
  * p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
  * p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>
  *
  * ------destroyed by collision------
  * ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
  * ------destroyed by collision------                      (3)
  * p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>
  * }}}
  * 
  * In this example, particles 0, 1, and 2 are simultaneously
  * destroyed at the time and place marked X. On the next tick,
  * particle 3 passes through unharmed.
  *
  * How many particles are left after all collisions are resolved?
  */
object ParticleSwarm extends AdventOfCodeDay {
  type P1 = Int
  type P2 = Int

  implicit val particleEq: Eq[Particle] = cats.derive.eq[Particle]
  implicit val particleShow: Show[Particle] = cats.derive.show[Particle]

  def runPart1(input: String): Try[P1] =
    Part1.closestToOrigin(input.trim, 1000).mapError(ParticleSwarm, input)

  def runPart2(input: String): Try[P2] =
    Part1.parse(input.trim)
      .map(Part2.simulate(_, 1000))
      .map(_.size)
      .mapError(ParticleSwarm, input)

  def verifyPart1Samples(): Unit = {
    import Part1.ParticleOps

    val input = """|p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
                   |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>
                   |p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>
                   |p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>
                   |p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>
                   |p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>
                   |p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>
                   |p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>
                   |""".stripMargin

    val particle0 = Seq(
      Particle(Point( 3,0,0), Velocity( 2,0,0), Acceleration(-1,0,0)),
      Particle(Point( 4,0,0), Velocity( 1,0,0), Acceleration(-1,0,0)),
      Particle(Point( 4,0,0), Velocity( 0,0,0), Acceleration(-1,0,0)),
      Particle(Point( 3,0,0), Velocity(-1,0,0), Acceleration(-1,0,0)))

    val particle1 = Seq(
      Particle(Point( 4,0,0), Velocity( 0,0,0), Acceleration(-2,0,0)),
      Particle(Point( 2,0,0), Velocity(-2,0,0), Acceleration(-2,0,0)),
      Particle(Point(-2,0,0), Velocity(-4,0,0), Acceleration(-2,0,0)),
      Particle(Point(-8,0,0), Velocity(-6,0,0), Acceleration(-2,0,0)))

    println("--- parsing ---")
    val expected = particle0.zip(particle1).flatMap {
      case (p0, p1) => Vector(p0, p1)
    }

    input.split('\n').zip(expected)
      .map((verifyResult(Parser.particle.tryToParse(_)) _).tupled)
      .foreach(println)

    println(verifyResult(Part1.parse)(input, expected.toVector))

    println("--- stepping ---")
    particle0.sliding(2).map {
      case Seq(p, pnext) => Try(p.step).asResult(pnext).show
    }.foreach(println)

    particle1.sliding(2).map {
      case Seq(p, pnext) => Try(p.step).asResult(pnext).show
    }.foreach(println)

    println("--- distance to origin ---")
    expected.zip(expected.map(_.point.x.abs))
      .map {
        case (particle, expectedDistance) =>
          Try(particle.distanceToOrigin).asResult(expectedDistance).show
      }
      .foreach(println)

    println("--- closest to origin ---")
    println(verifyResult(Part1.closestToOrigin(_: String, 1000))(
      """|p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
         |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>""".stripMargin,
      0))
  }

  def verifyPart2Samples(): Unit = {
    val initial = Vector(
      Particle(Point(-6,0,0), Velocity( 3,0,0), Acceleration(0,0,0)),
      Particle(Point(-4,0,0), Velocity( 2,0,0), Acceleration(0,0,0)),
      Particle(Point(-2,0,0), Velocity( 1,0,0), Acceleration(0,0,0)),
      Particle(Point( 3,0,0), Velocity(-1,0,0), Acceleration(0,0,0)))

    val step1 = Vector(
      0 -> Particle(Point(-3,0,0), Velocity( 3,0,0), Acceleration(0,0,0)),
      1 -> Particle(Point(-2,0,0), Velocity( 2,0,0), Acceleration(0,0,0)),
      2 -> Particle(Point(-1,0,0), Velocity( 1,0,0), Acceleration(0,0,0)),
      3 -> Particle(Point( 2,0,0), Velocity(-1,0,0), Acceleration(0,0,0)))

    val step2 = Vector(
      3 -> Particle(Point(1,0,0), Velocity(-1,0,0), Acceleration(0,0,0)))

    val step3 = Vector(
      3 -> Particle(Point(0,0,0), Velocity(-1,0,0), Acceleration(0,0,0)))

    println(Try(Part2.simulate(initial, 1).sortBy(_._1)).asResult(step1).show)
    println(Try(Part2.simulate(initial, 2).sortBy(_._1)).asResult(step2).show)
    println(Try(Part2.simulate(initial, 3).sortBy(_._1)).asResult(step3).show)
  }
}
