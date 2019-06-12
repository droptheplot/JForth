import java.io.FileOutputStream

import cats.effect._
import org.objectweb.asm._

import scala.language.higherKinds

object Main extends IOApp with Opcodes {
  def run(args: List[String]): IO[ExitCode] = {
    val node: List[Node] = List[Node](
      Node(Const(5)),
      Node(Command("dup")),
      Node(Command("iadd")),
      Node(Const(100)),
      Node(Command("swap")),
      Node(Command("idiv")),
    )

    val byteCode: Array[Byte] = Compiler.run(node)

    IO { new FileOutputStream("Hello.class") }
      .map(_.write(byteCode))
      .map(_ => ExitCode.Success)
  }
}
