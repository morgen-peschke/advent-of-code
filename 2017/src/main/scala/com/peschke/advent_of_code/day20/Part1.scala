package com.peschke.advent_of_code
package day20

import scala.util.Try

import cats.Traverse
import cats.instances.vector._
import cats.instances.try_._

object Part1 {
  def parse(input: String): Try[Vector[Particle]] =
    Traverse[Vector].traverse(input.split('\n').toVector)(Parser.particle.tryToParse(_))

  implicit class AccelerationOps(val a: Acceleration) extends AnyVal {
    def apply(v: Velocity): Velocity =
      (a, v) match {
        case (Acceleration(ax, ay, az), Velocity(vx, vy, vz)) =>
          Velocity(vx + ax, vy + ay, vz + az)
      }
  }

  implicit class VelocityOps(val v: Velocity) extends AnyVal {
    def apply(p: Point): Point =
      (v, p) match {
        case (Velocity(vx, vy, vz), Point(x, y, z)) => Point(x + vx, y + vy, z + vz)
      }
  }

  implicit class PointOps(val p: Point) extends AnyVal {
    def distanceToOrigin: Int = p.x.abs + p.y.abs + p.z.abs
  }

  implicit class ParticleOps(val p: Particle) extends AnyVal {
    def accelerate: Particle = p.copy(velocity = p.acceleration(p.velocity))
    def move: Particle = p.copy(point = p.velocity(p.point))
    def step: Particle = p.accelerate.move
    def distanceToOrigin: Int = p.point.distanceToOrigin
  }

  def closestToOrigin(input: String, iterations: Int): Try[Int] =
    parse(input)
      .map { particles =>
        (0 to iterations)
          .foldLeft(particles) { (particles, _) => particles.map(_.step) }
          .zipWithIndex
          .map {
            case (particle, index) => (particle.distanceToOrigin, index)
          }
          .minBy(_._1)
          ._2
      }
}
