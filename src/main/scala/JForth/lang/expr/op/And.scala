package JForth.lang.expr.op

import cats.data.State
import org.objectweb.asm._
import JForth.lang.Context

case class And() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(IAND)
  }
}

object And {
  val token: String = "and"
}
