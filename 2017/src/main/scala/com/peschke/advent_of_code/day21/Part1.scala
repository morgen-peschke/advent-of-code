package com.peschke.advent_of_code
package day21

import scala.util.Try

import cats.instances.try_._
import cats.instances.vector._
import cats.syntax.traverse._

object Part1 {
  implicit class ImageOps(val image: Image) extends AnyVal {
    def split: Image = image match {
      case Image.Divided(images) => Image.Divided(images.map(_.map(_.split)))
      case Image.Flat(pixels) =>
        val groupSize = pixels.length match {
          case s if s % 2 == 0 => 2
          case s if s % 3 == 0 => 3
          case s => throw new IllegalArgumentException(
            s"Image size must be a multiple of 2 or 3, was $s")
        }
        Image.Divided(
          pixels
            .grouped(groupSize)
            .toVector
            .map { row =>
              row.transpose
                .grouped(groupSize)
                .toVector
                .map(_.transpose)
                .map(Image.Flat)
            })
    }

    def transform(f: Image.Flat => Image): Image = image match {
      case Image.Divided(images) => Image.Divided(images.map(_.map(_.transform(f))))
      case i @ Image.Flat(_) => f(i)
    }

    def applyTransformation(t: Transformation): Image =
      transform { image =>
        t match {
          case Identity => image
          case FlipVertical => Image.Flat(image.pixels.reverse)
          case FlipHorizontal => Image.Flat(image.pixels.map(_.reverse))
          case Rotate90 => Image.Flat(image.pixels.transpose).applyTransformation(FlipHorizontal)
          case Rotate180 => image.applyTransformation(Rotate90 :: Rotate90 :: Nil)
          case Rotate270 => image.applyTransformation(Rotate90 :: Rotate90 :: Rotate90 :: Nil)
        }
    }

    def applyTransformation(t: Seq[Transformation]): Image =
      t.foldLeft(image)(_ applyTransformation _)

    def flatten: Image.Flat = image match {
      case f @ Image.Flat(_) => f
      case Image.Divided(images) =>
        Image.Flat(
          images
            .map { rowOfImages =>
              rowOfImages.map(_.flatten.pixels.transpose).reduce(_ ++ _).transpose
            }
            .reduce(_ ++ _)
        )
    }
  }

  val defaultImage: Image = {
    import Pixel.{On,Off}
    Image(
      Vector(Off, On, Off),
      Vector(Off, Off, On),
      Vector(On , On , On))
  }

  def transformUsing(rules: Seq[Rule])(image: Image): Image =
    image
      .split
      .transform { subImage =>
        Transformation
          .standard
          .toStream
          .map(subImage.applyTransformation)
          .flatMap { transformed =>
            rules.find(_.pattern == transformed)
          }
          .map(_.result)
          .headOption
          .getOrElse { throw new IllegalArgumentException(
            Seq(
              "Unable to find a matching pattern in rules",
              "--- Rules ---",
              rules.map(_.render).mkString("\n"),
              "--- Full Image ---",
              image.render,
              "--- Sub-image ---",
              subImage.render).mkString("\n"))
          }
      }
      .flatten

  def litPixelsAfterIterations(iterations: Int)(input: String): Try[Int] =
    input
      .split('\n')
      .toVector
      .traverse(Parser.rule.tryToParse(_))
      .map { rules =>
        (0 until iterations)
          .foldLeft(defaultImage) { (img, _) =>
            transformUsing(rules)(img)
          }
          .flatten
          .pixels
          .flatten
          .collect {
            case Pixel.On => 1
          }
          .sum
      }
}
