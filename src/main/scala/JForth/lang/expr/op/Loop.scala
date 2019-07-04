package JForth.lang.expr.op

import cats.data.State
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}
import JForth.lang.Context
import JForth.lang.expr.{Defn, Expr, Load}

case class Loop(end: Int, start: Int, exprs: Seq[Expr[String]]) extends Expr[String] {

  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State[Context[String], Unit] { ctx =>
    val startLabel: Label = new Label
    val endLabel: Label   = new Label

    val varIndex: Int = ctx.varIndex.next

    mv.visitIntInsn(BIPUSH, start)
    mv.visitVarInsn(ISTORE, varIndex)
    mv.visitLabel(startLabel)
    mv.visitVarInsn(ILOAD, varIndex)
    mv.visitIntInsn(BIPUSH, end)

    mv.visitJumpInsn(IF_ICMPGE, endLabel)

    exprs
      .foldLeft(State.pure[Context[String], Unit](())) {
        case (s, e) => s.flatMap(_ => e.run(mv))
      }
      .run(
        Context[String](
          defns = Map[String, Defn[String]](
            "i" -> Defn("i", Seq[Expr[String]](Load[String](varIndex)))
          )))
      .value

    mv.visitIincInsn(varIndex, 1)

    mv.visitJumpInsn(GOTO, startLabel)
    mv.visitLabel(endLabel)

    (ctx, ())
  }
}
