import java.io.FileOutputStream

import org.objectweb.asm._
import cats.effect._

import scala.language.higherKinds

object Main extends IOApp with Opcodes {
  def run(args: List[String]): IO[ExitCode] = {
    val source: String        = args.mkString(" ")
    val tokens: Seq[String]   = Tokenizer(source)
    val nodes: Seq[Node]      = Parser(tokens)
    val byteCode: Array[Byte] = Compiler.run(nodes)

    IO { new FileOutputStream("Hello.class") }
      .map(_.write(byteCode))
      .attempt
      .map {
        case Left(_)  => ExitCode.Error
        case Right(_) => ExitCode.Success
      }
  }
}
