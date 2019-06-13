import org.objectweb.asm._

object Compiler {
  import Opcodes._

  def run(nodes: Seq[Node]): Array[Byte] = {
    val cw: ClassWriter   = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    var mv: MethodVisitor = null
    val index: Int        = 2 // FIXME

    file(cw, "Hello") // FIXME

    mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC,
                        "main",
                        "([Ljava/lang/String;)V",
                        null,
                        null)

    nodes.foreach(_.run(mv))

    mv.visitVarInsn(ISTORE, index)

    print(mv, index)

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

  private def print(mv: MethodVisitor, index: Int): Unit = {
    mv.visitFieldInsn(GETSTATIC,
                      "java/lang/System",
                      "out",
                      "Ljava/io/PrintStream;")

    mv.visitVarInsn(ILOAD, index)

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
}
