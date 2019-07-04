package JForth

import java.io.FileOutputStream

import cats.effect.{ExitCode, IO, IOApp}
import fastparse.Parsed.{Failure, Success}
import org.objectweb.asm.Opcodes
import JForth.lang.{Compiler, Parser}

object Main extends IOApp with Opcodes {
  def run(args: List[String]): IO[ExitCode] =
    Parser(args.mkString(" ")) match {
      case Failure(_, _, extra) =>
        for {
          _ <- IO { println(extra.toString) }
        } yield ExitCode.Error
      case Success(exprs, _) =>
        for {
          _ <- IO { println(exprs.toString) }
          byteArray = Compiler.run(exprs)
          exitCode <- IO { new FileOutputStream("Hello.class") }
            .map(_.write(byteArray))
            .attempt
            .map {
              case Left(_)  => ExitCode.Error
              case Right(_) => ExitCode.Success
            }
        } yield exitCode
    }
}
