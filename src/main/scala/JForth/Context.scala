package JForth

import org.objectweb.asm.Label

case class Context[T](defns: Defns[T] = Map[String, Defn[T]](), label: Option[Label] = None)
