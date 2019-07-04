package JForth.lang.expr.op

import cats.data.State
import org.objectweb.asm._
import JForth.lang.{Context, Syntax}

case class Then() extends Op[String] with Syntax {
  def run(mv: MethodVisitor): State[Context[String], Unit] = State[Context[String], Unit] { ctx =>
    ctx.label match {
      case Some(endLabel) =>
        mv.visitLabel(endLabel)
    }

    (ctx.copy(label = None), ())
  }
}

object Then {
  val token: String = "then"
}
