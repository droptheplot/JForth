package JForth.lang.expr.op

import cats.data.State
import org.objectweb.asm._
import JForth.lang.{Context, Syntax}

case class If() extends Op[String] with Syntax {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State[Context[String], Unit] { ctx =>
    val endLabel: Label = new Label

    mv.visitIntInsn(BIPUSH, TRUE)
    mv.visitJumpInsn(IF_ICMPNE, endLabel)

    (ctx.copy(label = Some(endLabel)), ())
  }
}

object If {
  val token: String = "if"
}
