package JForth

import cats.data.State
import org.objectweb.asm._

import scala.util.Try

sealed trait Expr[T] {
  def run(mv: MethodVisitor): State[Map[String, Defn[T]], Unit]
}

case class Atom[T](value: T) extends Expr[T] {

  import Opcodes._

  def run(mv: MethodVisitor): State[Map[String, Defn[T]], Unit] =
    State[Map[String, Defn[T]], Unit] { defns =>
      value match {
        case v: String if Try(v.toInt).isSuccess =>
          mv.visitIntInsn(BIPUSH, v.toInt)

          (defns, ())
        case v: String =>
          defns(v).exprs
            .foldLeft(State.set[Map[String, Defn[T]]](defns)) { (state, expr) =>
              state.flatMap { _ =>
                expr.run(mv)
              }
            }
            .run(defns)
            .value
        case v =>
          mv.visitLdcInsn(v)

          (defns, ())
      }
    }
}

case class Defn[T](value: String, exprs: Seq[Expr[T]]) extends Expr[T] {
  def run(mv: MethodVisitor): State[Defns[T], Unit] = State[Defns[T], Unit] { defns =>
    (defns + (value -> Defn(value, exprs)), ())
  }
}

case class Op[T](value: T) extends Expr[T] with Syntax {
  import Opcodes._

  override def run(mv: MethodVisitor): State[Defns[T], Unit] = State[Defns[T], Unit] { defns =>
    value match {
      case "+"    => mv.visitInsn(IADD)
      case "*"    => mv.visitInsn(IMUL)
      case "-"    => mv.visitInsn(ISUB)
      case "/"    => mv.visitInsn(IDIV)
      case "dup"  => mv.visitInsn(DUP)
      case "pop"  => mv.visitInsn(POP)
      case "swap" => mv.visitInsn(SWAP)
      case "and"  => mv.visitInsn(IAND)
      case "or"   => mv.visitInsn(IOR)
      case "=" | "<" | ">" =>
        val elseLabel: Label = new Label
        val endLabel: Label  = new Label

        value match {
          case "=" => mv.visitJumpInsn(IF_ICMPNE, elseLabel)
          case "<" => mv.visitJumpInsn(IF_ICMPGE, elseLabel)
          case ">" => mv.visitJumpInsn(IF_ICMPLE, elseLabel)
        }

        mv.visitIntInsn(BIPUSH, TRUE)
        mv.visitJumpInsn(GOTO, endLabel)
        mv.visitLabel(elseLabel)
        mv.visitIntInsn(BIPUSH, FALSE)
        mv.visitLabel(endLabel)
      case "." =>
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
                           "println",
                           "(Ljava/lang/String;)V",
                           false)
    }

    (defns, ())
  }
}
