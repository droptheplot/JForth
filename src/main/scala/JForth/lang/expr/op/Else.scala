package JForth.lang.expr.op

import cats.data.State
import org.objectweb.asm._
import JForth.lang.{Context, Syntax}

case class Else() extends Op[String] with Syntax {
  import Opcodes._

  val token: String = "else"

  def run(mv: MethodVisitor): State[Context[String], Unit] = State[Context[String], Unit] { ctx =>
    ctx.label match {
      case Some(endLabel) =>
        val newEndLabel: Label = new Label

        mv.visitJumpInsn(GOTO, newEndLabel)
        mv.visitLabel(endLabel)

        (ctx.copy(label = Some(newEndLabel)), ())
      case _ =>
        (ctx, ())
    }
  }
}

object Else {
  val token: String = "else"
}
