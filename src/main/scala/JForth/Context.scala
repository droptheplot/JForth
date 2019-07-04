package JForth

import org.objectweb.asm.Label

/** Contains data for State
  *
  *  @param label current Label for if/else/then expressions
  *  @param varIndex current unused index for visitVarInsn, should be incremented after use
  */
case class Context[T](defns: Defns[T] = Map[String, Defn[T]](),
                      label: Option[Label] = None,
                      varIndex: Iterator[Int] = Iterator.from(0))
