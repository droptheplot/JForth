package JForth.lang

import cats.data.{Kleisli, State}
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}
import JForth.{CompilerLike, Config}
import JForth.lang.expr.Expr
import cats.effect.IO

class Compiler extends CompilerLike {
  import Opcodes._

  def apply(exprs: Seq[Expr[String]]): Kleisli[IO, Config, Array[Byte]] =
    Kleisli[IO, Config, Array[Byte]] { config =>
      val cw: ClassWriter   = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
      var mv: MethodVisitor = null

      file(cw, config.name.getOrElse(""))

      mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)

      exprs
        .foldLeft(State.pure[Context[String], Unit](())) {
          case (s, e) => s.flatMap(_ => e.run(mv))
        }
        .run(Context[String]())
        .value

      mv.visitInsn(RETURN)
      mv.visitMaxs(0, 0)
      mv.visitEnd()

      cw.visitEnd()

      IO.pure(cw.toByteArray)
    }

  private def file(cw: ClassWriter, name: String): Unit = {
    cw.visit(
      52,
      ACC_PUBLIC + ACC_SUPER,
      name,
      null,
      "java/lang/Object",
      null
    )

    cw.visitSource(s"$name.java", null)
  }
}
