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

    val innerCtx: State[Context[String], Unit] = State.set[Context[String]](
      ctx.copy(ctx.defns + ("i" -> Defn("i", Seq[Expr[String]](Load[String](varIndex))))))

    exprs
      .foldLeft(innerCtx) { (state, expr) =>
        state.flatMap(_ => expr.run(mv))
      }
      .run(ctx)
      .value

    mv.visitIincInsn(varIndex, 1)

    mv.visitJumpInsn(GOTO, startLabel)
    mv.visitLabel(endLabel)

    (ctx, ())
  }
}
