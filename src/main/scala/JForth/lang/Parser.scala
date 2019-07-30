package JForth.lang

import JForth.lang.expr.{Atom, Defn, Expr, Output}
import JForth.lang.expr.op._
import JForth.lang.expr.op.cond.{Eq, Ge, Le}
import JForth.{Config, ParserLike}
import cats.data.Kleisli
import cats.effect.IO

class Parser extends ParserLike {
  import fastparse._
  import SingleLineWhitespace._

  def space[_: P]: P[Unit]  = P { CharsWhileIn(" \r\n\t,").? }
  def number[_: P]: P[Unit] = P { CharsWhileIn("0-9") }
  def word[_: P]: P[Unit]   = P { CharsWhileIn("a-zA-Z0-9_\\-!?") }

  def op[_: P]: P[Op[String]] = P {
    (Add.token | Mul.token | Sub.token | Div.token | Dup.token |
      Pop.token | Swap.token | Eq.token | Le.token | Ge.token | And.token | Or.token | Mod.token |
      Invert.token | Print.token | If.token | Else.token | Then.token | Cr.token).!.map(
      Op.fromToken)
  }
  def atom[_: P]: P[Atom[String]] = P { (word | number).!.map(Atom[String]) }
  def output[_: P]: P[Output[String]] = P {
    ".\"" ~ CharPred(_ != '"').rep.!.map(Output[String]) ~ "\""
  }
  def loop[_: P]: P[Loop] = P {
    (number.!.map(_.toInt) ~ number.!.map(_.toInt) ~
      "do" ~ ((output | op | !"loop" ~ atom).rep(1, space) ~ "loop"))
      .map((Loop.apply _).tupled)
  }

  def definition[_: P]: P[Defn[String]] =
    P {
      (":" ~ atom ~
        (loop.map(Seq[Expr[String]](_)) | (output | op | atom).rep(1, space)) ~ ";").map {
        case (Atom(v), e) => Defn(v, e)
      }
    }

  def parser[_: P]: P[Seq[Expr[String]]] =
    P { (output | op | atom | definition).rep(1, space) }

  def apply(source: String): Kleisli[IO, Config, Parsed[Seq[Expr[String]]]] =
    Kleisli[IO, Config, Parsed[Seq[Expr[String]]]](_ => IO.pure(parse(source, parser(_))))
}
