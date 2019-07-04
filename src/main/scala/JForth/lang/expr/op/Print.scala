package JForth.lang.expr.op

import cats.data.State
import org.objectweb.asm._
import JForth.lang.Context

case class Print() extends Op[String] {
  import Opcodes._

  override def run(mv: MethodVisitor): State[Context[String], Unit] =
    State[Context[String], Unit] { ctx =>
      val varIndex: Int = ctx.varIndex.next

      mv.visitVarInsn(ISTORE, varIndex)

      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

      mv.visitVarInsn(ILOAD, varIndex)

      mv.visitMethodInsn(INVOKESTATIC,
                         "java/lang/Integer",
                         "valueOf",
                         "(I)Ljava/lang/Integer;",
                         false)
      mv.visitMethodInsn(INVOKEVIRTUAL,
                         "java/lang/Integer",
                         "toString",
                         "()Ljava/lang/String;",
                         false)

      mv.visitMethodInsn(INVOKEVIRTUAL,
                         "java/io/PrintStream",
                         "print",
                         "(Ljava/lang/String;)V",
                         false)

      (ctx, ())
    }
}

object Print {
  val token: String = "."
}
