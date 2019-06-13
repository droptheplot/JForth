import org.objectweb.asm._

object Compiler {
  import Opcodes._

  def run(nodes: Seq[Node]): Array[Byte] = {
    val cw: ClassWriter   = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    var mv: MethodVisitor = null

    file(cw, "Hello") // FIXME

    mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC,
                        "main",
                        "([Ljava/lang/String;)V",
                        null,
                        null)

    nodes.foreach(_.run(mv))

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
