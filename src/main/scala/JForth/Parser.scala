package JForth

object Parser {
  import fastparse._
  import SingleLineWhitespace._

  def space[_: P]: P[Unit]  = P { CharsWhileIn(" \r\n\t,").? }
  def number[_: P]: P[Unit] = P { CharsWhileIn("0-9") }
  def word[_: P]: P[Unit]   = P { CharsWhileIn("a-zA-Z0-9_\\-!?") }

  def op[_: P]: P[Op[String]] = P {
    (Add.token | Mul.token | Sub.token | Div.token | Dup.token |
      Pop.token | Swap.token | Eq.token | Le.token | Ge.token | And.token | Or.token | Mod.token |
      Invert.token | Print.token | If.token | Else.token | Then.token).!.map(Op.fromToken)
  }
  def atom[_: P]: P[Atom[String]] = P { (word | number).!.map(Atom[String]) }
  def output[_: P]: P[Output[String]] = P {
    ".\"" ~ CharPred(_ != '"').rep.!.map(Output[String]) ~ "\""
  }

  def definition[_: P]: P[Defn[String]] =
    P {
      (":" ~ atom ~ (output | op | atom).rep(1, space) ~ ";").map {
        case (Atom(v), e) => Defn(v, e)
      }
    }

  def parser[_: P]: P[Seq[Expr[String]]] =
    P { (output | op | atom | definition).rep(1, space) }

  def apply(source: String): Parsed[Seq[Expr[String]]] =
    parse(source, parser(_))
}
