package JForth.lang

import org.objectweb.asm.Label
import JForth.lang.expr.Defn

/** Contains data for State
  *
  *  @param label current Label for if/else/then expressions
  *  @param varIndex current unused index for visitVarInsn, should be incremented after use
  */
case class Context[T](defns: Map[String, Defn[T]] = Map[String, Defn[T]](),
                      label: Option[Label] = None,
                      varIndex: Iterator[Int] = Iterator.from(0))
