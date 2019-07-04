package JForth.lang.expr.op.cond

case class Le() extends Cond[String] {}

object Le {
  val token: String = "<"
}
