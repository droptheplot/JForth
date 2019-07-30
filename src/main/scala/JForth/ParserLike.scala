package JForth

import cats.data.Kleisli
import cats.effect.IO
import fastparse.Parsed

trait ParserLike {
  def apply(source: String): Kleisli[IO, Config, Parsed[Seq[lang.expr.Expr[String]]]]
}
