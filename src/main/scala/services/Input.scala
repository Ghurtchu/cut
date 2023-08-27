package services

import java.nio.file.{Files, Path}
import scala.util.Try

trait Input {
  def load: String
}

object Input {
  def fromFile(filename: String): Input = new Input {
    override def load: String =
      Try(Files readString (Path of filename))
        .getOrElse(throw new RuntimeException("File does not exist"))
  }
  def fromStdIn: Input                  = new Input {
    override def load: String =
      scala.io.Source.stdin.getLines
        .mkString("\n")
  }

}
