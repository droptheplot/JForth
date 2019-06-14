object Parser {
  import fastparse._, SingleLineWhitespace._

  def space[_: P]: P[Unit]        = P { CharsWhileIn(" \r\n\t,").? }
  def number[_: P]: P[Unit]       = P { CharsWhileIn("0-9") }
  def operator[_: P]: P[Unit]     = P { CharsWhileIn("+\\-*/.<=>") }
  def char[_: P]: P[Unit]         = P { CharsWhileIn("a-z") }
  def atom[_: P]: P[Atom[String]] = P { (char | number).!.map(Atom[String]) }
  def op[_: P]: P[Op[String]]     = P { operator.!.map(Op[String]) }

  def definition[_: P]: P[Defn[String]] =
    P { (":" ~ atom ~ atom.rep(1, space) ~ ";").map { case (Atom(v), e) => Defn(v, e) } }

  def parser[_: P]: P[Seq[Expr[String]]] =
    P { (atom | op | definition).rep(1, space) }

  def apply(source: String): Parsed[Seq[Expr[String]]] =
    parse(source, parser(_))
}
