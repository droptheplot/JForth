package JForth.lang

import cats.data.State
import JForth.lang
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}
import JForth.lang.expr.Expr

object Compiler {
  import Opcodes._

  def run(exprs: Seq[Expr[String]]): Array[Byte] = {
    val cw: ClassWriter   = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    var mv: MethodVisitor = null

    file(cw, "Hello") // FIXME

    mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)

    exprs
      .foldLeft(State.pure[Context[String], Unit](())) {
        case (s, e) => s.flatMap(_ => e.run(mv))
      }
      .run(lang.Context[String]())
      .value

    mv.visitInsn(RETURN)
    mv.visitMaxs(0, 0)
    mv.visitEnd()

    cw.visitEnd()

    cw.toByteArray
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
