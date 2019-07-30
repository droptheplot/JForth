package JForth

import java.io.{File, FileOutputStream}

import cats.effect.{ExitCode, IO, IOApp}
import fastparse.Parsed.{Failure, Success}
import org.objectweb.asm.Opcodes
import scopt.{OParser, OParserBuilder}

import scala.io.Source

object Main extends IOApp with Opcodes {
  val initConfig: Config = Config(parser = new lang.Parser, compiler = new lang.Compiler)

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
    for {
      config <- IO.fromEither(
        OParser.parse(parser, args, initConfig) match {
          case Some(a) => Right(a)
          case None    => Left(new Exception("Cannot parse arguments"))
        }
      )
      source = config.input.get match {
        case Left(i) => i
        case Right(f) =>
          val s = Source.fromFile(f)
          try s.getLines.mkString
          finally s.close
      }
      exprs <- config.parser(source).run(config).flatMap {
        case Success(value, _)    => IO.pure(value)
        case Failure(_, _, extra) => IO.raiseError(new Exception(extra.trace(false).msg))
      }
      byteArray <- config.compiler(exprs).run(config)
      exitCode  <- IO { new FileOutputStream(config.output.get).write(byteArray) }.attempt.toExitCode
    } yield exitCode
}
