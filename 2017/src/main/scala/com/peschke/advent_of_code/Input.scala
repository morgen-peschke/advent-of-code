package com.peschke.advent_of_code

import java.io.InputStream
import java.nio.file.{Files, Path, Paths}

import scopt.Read

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
      try slurp(reader) //
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
      case pathStr if pathStr.startsWith("file://") => FromPath(Paths.get(pathStr.drop(7)))
      case other => FromString(other)
    }
}
