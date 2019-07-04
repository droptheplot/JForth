package JForth.lang.expr

import cats.data.State
import JForth.lang.Context
import org.objectweb.asm.{MethodVisitor, Opcodes}

/** For internal use only */
case class Load[T](index: Int) extends Expr[T] {

  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] = State.pure[Context[T], Unit] {
    mv.visitVarInsn(ILOAD, index)
  }
}
