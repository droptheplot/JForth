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
        case "pop"  => mv.visitInsn(DUP)
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
      }
  }
}
