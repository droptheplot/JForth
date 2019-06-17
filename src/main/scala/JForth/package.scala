import cats.Applicative
import cats.data.State

package object JForth {
  type Defns[T] = Map[String, Defn[T]]
}
