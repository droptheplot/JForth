package JForth.lang.expr.op

import JForth.lang._
import JForth.lang.expr.op.cond.{Eq, Ge, Le}
import JForth.lang.expr.Expr

trait Op[T] extends Expr[T]

object Op {
  def fromToken(t: String): Op[String] = t match {
    case Add.token    => Add()
    case Mul.token    => Mul()
    case Sub.token    => Sub()
    case Div.token    => Div()
    case Dup.token    => Dup()
    case Pop.token    => Pop()
    case Swap.token   => Swap()
    case Eq.token     => Eq()
    case Le.token     => Le()
    case Ge.token     => Ge()
    case And.token    => And()
    case Or.token     => Or()
    case Mod.token    => Mod()
    case Invert.token => Invert()
    case Print.token  => Print()
    case If.token     => If()
    case Else.token   => Else()
    case Then.token   => Then()
    case Cr.token     => Cr()
  }
}
