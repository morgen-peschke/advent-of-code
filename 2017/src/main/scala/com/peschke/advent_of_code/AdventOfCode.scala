package com.peschke.advent_of_code

import java.io.InputStream
import java.nio.file.{Files, Path, Paths}

import scala.util.{Failure, Success}

import scopt.{OptionParser, Read}

object AdventOfCodeOpts {
  sealed trait Input {
    private def slurp(source: InputStream, chunkSize: Int = 1024): String =
      Iterator
        .continually {
          val buffer = Array.ofDim[Byte](chunkSize)
          val charsRead = source.read(buffer)
          if (charsRead == -1) Option.empty[Array[Byte]]
          else Option(buffer)
        }
        .takeWhile(_.isDefined)
        .collect {
          case Some(buffer) => buffer.map(_.toChar).mkString
        }
        .mkString


    def contents: String = this match {
      case Input.FromString(value) => value
      case Input.FromPath(path) =>
        val reader = Files.newInputStream(path)
        try slurp(reader)
        finally reader.close()
      case Input.FromStdIn =>
        try slurp(System.in)
        finally System.in.close()
    }
  }
  object Input {
    case class FromString(value: String) extends Input
    case class FromPath(value: Path) extends Input
    case object FromStdIn extends Input

    implicit val inputReads: Read[Input] =
      Read.reads {
        case "stream://stdin" => FromStdIn
        case pathStr if pathStr.startsWith("file://") => FromPath(Paths.get(pathStr.drop(8)))
        case other => FromString(other)
      }
  }

  sealed trait DayOpts {
    def as[T](implicit converter: OptsConverter[T]): T = converter.convertFrom(this)
  }

  trait OptsConverter[T] {
    def convertFrom(source: DayOpts): T
  }
  object OptsConverter {
    def apply[T](convert: DayOpts => T): OptsConverter[T] = new OptsConverter[T] {
      def convertFrom(source: DayOpts): T = convert(source)
    }
  }

  object DayOpts {
    case object NoDayChosen extends DayOpts

    case class InverseCaptchaOpts(input: Vector[Input], verifySamples: Boolean) extends DayOpts {
      def addInput(i: Input): InverseCaptchaOpts = copy(input = input :+ i)
    }

    object InverseCaptchaOpts {
      implicit val convertFromOpts: OptsConverter[InverseCaptchaOpts] =
        OptsConverter[InverseCaptchaOpts] {
          case i @ InverseCaptchaOpts(_, _) => i
          case _ => InverseCaptchaOpts(Vector.empty, false)
        }
    }
  }

  val optParser: OptionParser[DayOpts] = new OptionParser[DayOpts]("AdventOfCode") {
    head("AdventOfCode", "2017")

    cmd("inverseCaptcha")
      .action((_, c) => c.as[DayOpts.InverseCaptchaOpts])
      .text("  Day 1: Inverse Captcha @ http://adventofcode.com/2017/day/1")
      .children(
        note(""),
        opt[Unit]("verify-samples")
          .text("verify the sample input, defaults to false unless no arguments are given")
          .optional
          .maxOccurs(1)
          .action((_, c) => c.as[DayOpts.InverseCaptchaOpts].copy(verifySamples = true)),
        arg[Input]("<input>...")
          .text("run the inverse captcha on this input string")
          .optional
          .unbounded
          .action((i, c) => c.as[DayOpts.InverseCaptchaOpts].addInput(i))
      )

    checkConfig {
      case DayOpts.NoDayChosen => failure("No day chosen")
      case _ => success
    }
  }

  def parse(args: Seq[String]): Option[DayOpts] = optParser.parse(args, DayOpts.NoDayChosen)
}

object AdventOfCode extends App {
  import AdventOfCodeOpts.DayOpts._

  AdventOfCodeOpts
    .parse(args.map {
      case "-" => "stream://stdin"
      case x => x
    })
    .foreach {
    case NoDayChosen => throw new IllegalStateException("Should never reach this line of code")
    case InverseCaptchaOpts(inputs, verifySamples) =>
      if (verifySamples || inputs.isEmpty) {
        Day1InverseCaptcha.verifySampleCases()
      }

      inputs.foreach { input =>
        Day1InverseCaptcha.inverseCaptcha(input.contents.trim) match {
          case Failure(ex) => throw ex
          case Success(output) => println(output)
        }
      }
  }
}
