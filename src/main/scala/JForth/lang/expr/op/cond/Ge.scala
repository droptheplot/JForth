package JForth.lang.expr.op.cond

case class Ge() extends Cond[String] {}

object Ge {
  val token: String = ">"
}
