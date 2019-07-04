package JForth.lang.expr

import cats.data.State
import JForth.lang.Context
import org.objectweb.asm._

import scala.util.Try

case class Atom[T](value: T) extends Expr[T] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] =
    State[Context[T], Unit] { ctx =>
      value match {
        case v: String if Try(v.toInt).isSuccess =>
          mv.visitIntInsn(BIPUSH, v.toInt)

          (ctx, ())
        case v: String =>
          ctx
            .defns(v)
            .exprs
            .foldLeft(State.set[Context[T]](ctx)) { (state, expr) =>
              state.flatMap { _ =>
                expr.run(mv)
              }
            }
            .run(ctx)
            .value
        case v =>
          mv.visitLdcInsn(v)

          (ctx, ())
      }
    }
}
