package com.peschke.advent_of_code
package day21

sealed trait Pixel extends Product with Serializable {
  def render: String = this match {
    case Pixel.On  => "#"
    case Pixel.Off => "."
  }
}

object Pixel {
  case object On extends Pixel
  case object Off extends Pixel
}

sealed trait Image {
  def render: String
}
object Image {
  case class Flat(pixels: Vector[Vector[Pixel]]) extends Image {
    def render: String = pixels.map(_.map(_.render).mkString).mkString("\n")
  }

  case class Divided(images: Vector[Vector[Image]]) extends Image {
    def render: String =
      images
        .map(renderRowOfImages)
        .mkString("\n\n")
  }

  def apply(rows: Vector[Pixel]*): Image = Image.Flat(rows.toVector)

  def renderRowOfImages(row: Vector[Image]): String = {
    val renderedImages: Vector[String] = row.map(_.render)
    val renderedImageLines: Vector[Array[String]] = renderedImages.map(_.split('\n'))
    val combined: Array[String] = renderedImageLines.reduce { (rawA, rawB) =>
      val maxLen = rawA.length max rawB.length
      val aLines = rawA.padTo(maxLen, "")
      val bLines = rawB.padTo(maxLen, "")
      val sep = Array.fill(maxLen)(" ")

      (aLines zip sep zip bLines).map {
        case ((a, s), b) => s"$a$s$b"
      }
    }
    combined.mkString("\n")
  }
}

case class Rule(pattern: Image, result: Image) {
  def render: String = {
    val rawPatternLines = pattern.render.split("\n")
    val rawResultLines  = result.render.split("\n")
    val rawSepLine      = Seq(" -> ")
    val maxLen = rawPatternLines.length max rawResultLines.length
    val patternLines = rawPatternLines.padTo(maxLen, " " * rawPatternLines.length)
    val resultLines  = rawResultLines.padTo(maxLen, " " * rawResultLines.length)
    val sepLines     = rawSepLine.padTo(maxLen, "    ")

    (patternLines zip sepLines zip resultLines )
      .map {
        case ((p, s), r) => s"$p$s$r"
      }
      .mkString("\n")
  }
}

sealed trait Transformation extends Product with Serializable
case object Identity extends Transformation
case object FlipVertical extends Transformation
case object FlipHorizontal extends Transformation
case object Rotate90 extends Transformation
case object Rotate180 extends Transformation
case object Rotate270 extends Transformation

object Transformation {
  val rotations: Seq[Transformation] =
    Seq(Identity, Rotate90, Rotate180, Rotate270)

  val flips: Seq[Transformation] =
    Seq(Identity, FlipVertical, FlipHorizontal)

  val standard: Seq[Seq[Transformation]] =
    for {
      r <- rotations
      f <- flips
    } yield Seq(r, f)
}

object Parser {
  import fastparse.all._

  val onPixel: P[Pixel] = P("#").map(_ => Pixel.On)
  val offPixel: P[Pixel] = P(".").map(_ => Pixel.Off)
  val pixel: P[Pixel] = onPixel | offPixel

  val line: P[Seq[Pixel]] = P(pixel.rep)
  val lineSep: P[Unit] = P("/")
  val image: P[Image] = P(line.rep(min = 1, sep = lineSep)).map { lines =>
    Image.Flat(lines.toVector.map(_.toVector))
  }

  val imageSep: P[Unit] = P(" => ")
  val rule: P[Rule] = P(image ~/ imageSep ~/ image).map(Rule.tupled)
}
