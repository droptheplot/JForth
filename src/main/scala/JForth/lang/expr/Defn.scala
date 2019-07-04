package JForth.lang.expr

import cats.data.State
import org.objectweb.asm.MethodVisitor
import JForth.lang.Context

case class Defn[T](value: String, exprs: Seq[Expr[T]]) extends Expr[T] {
  def run(mv: MethodVisitor): State[Context[T], Unit] = State[Context[T], Unit] { ctx =>
    (ctx.copy(ctx.defns + (value -> Defn(value, exprs))), ())
  }
}
