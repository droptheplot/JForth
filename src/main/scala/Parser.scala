import scala.util.matching.Regex

object Parser {
  val intRegex: Regex = "(\\d+)".r

  def apply(tokens: Seq[String]): Seq[Node] =
    tokens.map {
      case "+"         => Node(Command("iadd"))
      case "-"         => Node(Command("isub"))
      case "*"         => Node(Command("imul"))
      case "/"         => Node(Command("idiv"))
      case "dup"       => Node(Command("dup"))
      case "pop"       => Node(Command("pop"))
      case "swap"      => Node(Command("swap"))
      case "="         => Node(Command("="))
      case "<"         => Node(Command("<"))
      case ">"         => Node(Command(">"))
      case "and"       => Node(Command("and"))
      case "or"        => Node(Command("or"))
      case "."         => Node(Command("iprint"))
      case intRegex(i) => Node(Const(i.toInt))
      case v           => Node(Const(v))
    }
}
