import org.objectweb.asm._

sealed trait State
case class Const(value: Any)         extends State
case class Command(operator: String) extends State

case class Node(state: State) extends Syntax {
  import Opcodes._

  def run(mv: MethodVisitor): Unit = state match {
    case Const(value) =>
      value match {
        case value: String =>
          mv.visitLdcInsn(value)
        case v: Int =>
          mv.visitIntInsn(BIPUSH, v) // TODO should be newarray?
      }
    case Command(operator) =>
      operator match {
        case "iadd" => mv.visitInsn(IADD)
        case "imul" => mv.visitInsn(IMUL)
        case "isub" => mv.visitInsn(ISUB)
        case "idiv" => mv.visitInsn(IDIV)
        case "dup"  => mv.visitInsn(DUP)
        case "pop"  => mv.visitInsn(POP)
        case "swap" => mv.visitInsn(SWAP)
        case "and"  => mv.visitInsn(IAND)
        case "or"   => mv.visitInsn(IOR)
        case "=" | "<" | ">" =>
          val elseLabel: Label = new Label
          val endLabel: Label  = new Label

          operator match {
            case "=" => mv.visitJumpInsn(IF_ICMPNE, elseLabel)
            case "<" => mv.visitJumpInsn(IF_ICMPGE, elseLabel)
            case ">" => mv.visitJumpInsn(IF_ICMPLE, elseLabel)
          }

          mv.visitIntInsn(BIPUSH, TRUE)
          mv.visitJumpInsn(GOTO, endLabel)
          mv.visitLabel(elseLabel)
          mv.visitIntInsn(BIPUSH, FALSE)
          mv.visitLabel(endLabel)
        case "iprint" =>
          mv.visitVarInsn(ISTORE, 2)

          mv.visitFieldInsn(GETSTATIC,
                            "java/lang/System",
                            "out",
                            "Ljava/io/PrintStream;")

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
  }
}
