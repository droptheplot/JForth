package JForth.lang.expr.op.cond

import cats.data.State
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}
import JForth.lang.{Context, Syntax}
import JForth.lang.expr.op.Op

trait Cond[T] extends Op[T] with Syntax {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] = State.pure[Context[T], Unit] {
    val elseLabel: Label = new Label
    val endLabel: Label  = new Label

    this match {
      case _: Eq => mv.visitJumpInsn(IF_ICMPNE, elseLabel)
      case _: Le => mv.visitJumpInsn(IF_ICMPGE, elseLabel)
      case _: Ge => mv.visitJumpInsn(IF_ICMPLE, elseLabel)
    }

    mv.visitIntInsn(BIPUSH, TRUE)
    mv.visitJumpInsn(GOTO, endLabel)
    mv.visitLabel(elseLabel)
    mv.visitIntInsn(BIPUSH, FALSE)
    mv.visitLabel(endLabel)
  }
}
