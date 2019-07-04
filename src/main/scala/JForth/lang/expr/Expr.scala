package JForth.lang.expr

import org.objectweb.asm.MethodVisitor
import JForth.lang.Context
import JForth.lang.expr.op.cond._
import cats.data.State
import JForth.lang.expr.op._

/** Represents JForth AST
  *
  * [[Expr]]
  * - [[Atom]]
  * - [[Defn]]
  * - [[Load]]
  * - [[Op]]
  * - - [[Add]]
  * - - [[Mul]]
  * - - [[Sub]]
  * - - [[Div]]
  * - - [[Dup]]
  * - - [[Pop]]
  * - - [[Swap]]
  * - - [[And]]
  * - - [[Or]]
  * - - [[Mod]]
  * - - [[Print]]
  * - - [[If]]
  * - - [[Else]]
  * - - [[Then]]
  * - - [[Loop]]
  * - - [[Invert]]
  * - - [[Cond]]
  * - - - [[Eq]]
  * - - - [[Le]]
  * - - - [[Ge]]
  * - [[Output]]
  */
trait Expr[T] {
  def run(mv: MethodVisitor): State[Context[T], Unit]
}
