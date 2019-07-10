package JForth

import java.io.{File, FileOutputStream}

import cats.effect.{ExitCode, IO, IOApp}
import fastparse.Parsed.{Failure, Success}
import org.objectweb.asm.Opcodes
import JForth.lang.{Compiler, Parser}
import scopt.{OParser, OParserBuilder}

import scala.io.Source

object Main extends IOApp with Opcodes {
  val builder: OParserBuilder[Config] = OParser.builder[Config]
  val parser: OParser[Unit, Config] = {
    import builder._

    OParser.sequence(
      programName("jforth"),
      head("jforth", "0.1"),
      opt[String]('n', "name")
        .action((n, c) => c.copy(name = Some(n)))
        .required,
      opt[String]('o', "output")
        .action((o, c) => c.copy(output = Some(new File(o))))
        .required,
      opt[String]('i', "input")
        .action((i, c) => c.copy(input = Some(Left(i)))),
      opt[String]('f', "file")
        .action((f, c) => c.copy(input = Some(Right(new File(f))))),
      checkConfig { c =>
        if (c.input.isEmpty) failure("input not provided (use -i or -f)")
        else success
      }
    )
  }

  def run(args: List[String]): IO[ExitCode] =
    OParser.parse(parser, args, Config()) match {
      case Some(Config(Some(name), Some(input), Some(output))) =>
        val source: String = input match {
          case Left(i) => i
          case Right(f) =>
            val s = Source.fromFile(f)
            try s.getLines.mkString
            finally s.close
        }

        Parser(source) match {
          case Failure(_, _, extra) =>
            for {
              _ <- IO {
                println(extra.toString)
              }
            } yield ExitCode.Error
          case Success(exprs, _) =>
            for {
              _ <- IO {
                println(exprs.toString)
              }
              byteArray = Compiler.run(name, exprs)
              exitCode <- IO {
                new FileOutputStream(output)
              }.map(_.write(byteArray))
                .attempt
                .map {
                  case Left(_)  => ExitCode.Error
                  case Right(_) => ExitCode.Success
                }
            } yield exitCode
        }
      case _ =>
        IO.pure(ExitCode.Error)
    }
}
