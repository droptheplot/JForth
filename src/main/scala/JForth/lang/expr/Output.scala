package JForth.lang.expr

import cats.data.State
import org.objectweb.asm.{MethodVisitor, Opcodes}
import JForth.lang.Context

case class Output[T](value: T) extends Expr[T] {

  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] = State.pure[Context[T], Unit] {
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

    mv.visitLdcInsn(value)

    mv.visitMethodInsn(INVOKEVIRTUAL,
                       "java/io/PrintStream",
                       "print",
                       "(Ljava/lang/String;)V",
                       false)
  }
}
