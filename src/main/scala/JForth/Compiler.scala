package JForth

import cats.data.State
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}

object Compiler {
  import Opcodes._

  def run(exprs: Seq[Expr[String]]): Array[Byte] = {
    val cw: ClassWriter   = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    var mv: MethodVisitor = null

    file(cw, "Hello") // FIXME

    mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)

    val state: State[Defns[String], Unit] =
      exprs.foldLeft(State.pure[Map[String, Defn[String]], Unit](())) {
        case (s, e) =>
          s.flatMap(_ => e.run(mv))
      }

    println("state: ", state.run(Map[String, Defn[String]]()).value.toString)

    mv.visitInsn(RETURN)
    mv.visitMaxs(100, 100) // FIXME
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
