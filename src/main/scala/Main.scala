import java.io.FileOutputStream

import cats.effect._
import fastparse.Parsed.{Failure, Success}
import org.objectweb.asm._

import scala.language.higherKinds

object Main extends IOApp with Opcodes {
  def run(args: List[String]): IO[ExitCode] = {
    val source: String = args.mkString(" ")

    val byteCode: Array[Byte] =
      Parser(source) match {
        case Failure(_, _, extra) =>
          println(extra.toString)
          Array[Byte]()
        case Success(value, _) =>
          println(value.toString)
          Compiler.run(value)
      }

    IO { new FileOutputStream("Hello.class") }
      .map(_.write(byteCode))
      .attempt
      .map {
        case Left(_)  => ExitCode.Error
        case Right(_) => ExitCode.Success
      }
  }
}
