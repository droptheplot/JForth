package JForth

import cats.data.State
import org.objectweb.asm._

import scala.util.Try

/** Represents Forth AST
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
  * - [[Cond]]
  * - - [[Eq]]
  * - - [[Le]]
  * - - [[Ge]]
  * - [[Output]]
  */
sealed trait Expr[T] {
  def run(mv: MethodVisitor): State[Context[T], Unit]
}

case class Atom[T](value: T) extends Expr[T] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] =
    State[Context[T], Unit] { ctx =>
      value match {
        case v: String if Try(v.toInt).isSuccess =>
          mv.visitIntInsn(BIPUSH, v.toInt)

          (ctx, ())
        case v: String =>
          ctx
            .defns(v)
            .exprs
            .foldLeft(State.set[Context[T]](ctx)) { (state, expr) =>
              state.flatMap { _ =>
                expr.run(mv)
              }
            }
            .run(ctx)
            .value
        case v =>
          mv.visitLdcInsn(v)

          (ctx, ())
      }
    }
}

/** For internal use only */
case class Load[T](index: Int) extends Expr[T] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] = State.pure[Context[T], Unit] {
    mv.visitVarInsn(ILOAD, index)
  }
}

case class Defn[T](value: String, exprs: Seq[Expr[T]]) extends Expr[T] {
  def run(mv: MethodVisitor): State[Context[T], Unit] = State[Context[T], Unit] { ctx =>
    (ctx.copy(ctx.defns + (value -> Defn(value, exprs))), ())
  }
}

sealed trait Op[T] extends Expr[T]

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
  }
}

case class Add() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(IADD)
  }
}

object Add {
  val token: String = "+"
}

case class Mul() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(IMUL)
  }
}

object Mul {
  val token: String = "*"
}

case class Sub() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(ISUB)
  }
}

object Sub {
  val token: String = "-"
}

case class Div() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(IDIV)
  }
}

object Div {
  val token: String = "/"
}

case class Dup() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(DUP)
  }
}

object Dup {
  val token: String = "dup"
}

case class Pop() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(POP)
  }
}

object Pop {
  val token: String = "pop"
}

case class Swap() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(SWAP)
  }
}

object Swap {
  val token: String = "swap"
}

case class And() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(IAND)
  }
}

object And {
  val token: String = "and"
}

case class Or() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(IOR)
  }
}

object Or {
  val token: String = "or"
}

case class Mod() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(IREM)
  }
}

object Mod {
  val token: String = "mod"
}

case class Invert() extends Op[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State.pure[Context[String], Unit] {
    mv.visitInsn(ICONST_M1)
    mv.visitInsn(IXOR)
  }
}

object Invert {
  val token: String = "invert"
}

sealed trait Cond[T] extends Op[T] with Syntax {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] = State.pure[Context[T], Unit] {
    val elseLabel: Label = new Label
    val endLabel: Label  = new Label

    this match {
      case _: Eq => mv.visitJumpInsn(IF_ICMPNE, elseLabel)
      case _: Le => mv.visitJumpInsn(IF_ICMPGE, elseLabel)
      case _: Ge => mv.visitJumpInsn(IF_ICMPLE, elseLabel)
    }

    mv.visitIntInsn(BIPUSH, TRUE)
    mv.visitJumpInsn(GOTO, endLabel)
    mv.visitLabel(elseLabel)
    mv.visitIntInsn(BIPUSH, FALSE)
    mv.visitLabel(endLabel)
  }
}

case class Eq() extends Cond[String]

object Eq {
  val token: String = "="
}

case class Le() extends Cond[String] {}

object Le {
  val token: String = "<"
}

case class Ge() extends Cond[String] {}

object Ge {
  val token: String = ">"
}

case class Print() extends Op[String] {
  import Opcodes._

  override def run(mv: MethodVisitor): State[Context[String], Unit] =
    State.pure[Context[String], Unit] {
      mv.visitVarInsn(ISTORE, 2)

      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

      mv.visitVarInsn(ILOAD, 2)

      mv.visitMethodInsn(INVOKESTATIC,
                         "java/lang/Integer",
                         "valueOf",
                         "(I)Ljava/lang/Integer;",
                         false)
      mv.visitMethodInsn(INVOKEVIRTUAL,
                         "java/lang/Integer",
                         "toString",
                         "()Ljava/lang/String;",
                         false)

      mv.visitMethodInsn(INVOKEVIRTUAL,
                         "java/io/PrintStream",
                         "print",
                         "(Ljava/lang/String;)V",
                         false)
    }
}

object Print {
  val token: String = "."
}

case class If() extends Op[String] with Syntax {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State[Context[String], Unit] { ctx =>
    val endLabel: Label = new Label

    mv.visitIntInsn(BIPUSH, TRUE)
    mv.visitJumpInsn(IF_ICMPNE, endLabel)

    (ctx.copy(label = Some(endLabel)), ())
  }
}

object If {
  val token: String = "if"
}

case class Else() extends Op[String] with Syntax {
  import Opcodes._

  val token: String = "else"

  def run(mv: MethodVisitor): State[Context[String], Unit] = State[Context[String], Unit] { ctx =>
    ctx.label match {
      case Some(endLabel) =>
        val newEndLabel: Label = new Label

        mv.visitJumpInsn(GOTO, newEndLabel)
        mv.visitLabel(endLabel)

        (ctx.copy(label = Some(newEndLabel)), ())
      case _ =>
        (ctx, ())
    }
  }
}

object Else {
  val token: String = "else"
}

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

case class Loop(end: Int, start: Int, exprs: Seq[Expr[String]]) extends Expr[String] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[String], Unit] = State[Context[String], Unit] { ctx =>
    val startLabel: Label = new Label
    val endLabel: Label   = new Label

    mv.visitIntInsn(BIPUSH, start)
    mv.visitVarInsn(ISTORE, 3)
    mv.visitLabel(startLabel)
    mv.visitVarInsn(ILOAD, 3)
    mv.visitIntInsn(BIPUSH, end)

    mv.visitJumpInsn(IF_ICMPGE, endLabel)

    exprs
      .foldLeft(State.pure[Context[String], Unit](())) {
        case (s, e) => s.flatMap(_ => e.run(mv))
      }
      .run(
        Context[String](
          defns = Map[String, Defn[String]](
            "i" -> Defn("i", Seq[Expr[String]](Load[String](3)))
          )))
      .value

    mv.visitIincInsn(3, 1)

    mv.visitJumpInsn(GOTO, startLabel)
    mv.visitLabel(endLabel)

    (ctx, ())
  }
}

case class Output[T](value: T) extends Expr[T] {
  import Opcodes._

  def run(mv: MethodVisitor): State[Context[T], Unit] = State.pure[Context[T], Unit] {
    mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

    mv.visitLdcInsn(value)

    mv.visitMethodInsn(INVOKEVIRTUAL,
                       "java/io/PrintStream",
                       "println",
                       "(Ljava/lang/String;)V",
                       false)
  }
}
