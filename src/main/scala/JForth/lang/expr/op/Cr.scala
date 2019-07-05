package JForth.lang.expr.op

import org.objectweb.asm.{MethodVisitor, Opcodes}
import JForth.lang.Context
import cats.data.State

case class Cr() extends Op[String] {

  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

    mv.visitLdcInsn("\n")

    mv.visitMethodInsn(INVOKEVIRTUAL,
                       "java/io/PrintStream",
                       "print",
                       "(Ljava/lang/String;)V",
                       false)
  }
}

object Cr {
  val token: String = "cr"
}
