package JForth.lang.expr.op.cond

case class Eq() extends Cond[String]

object Eq {
  val token: String = "="
}
