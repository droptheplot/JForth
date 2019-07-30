package JForth

import cats.data.Kleisli
import cats.effect.IO

trait CompilerLike {
  def apply(exprs: Seq[lang.expr.Expr[String]]): Kleisli[IO, Config, Array[Byte]]
}
