package JForth

import org.scalatest.FunSpec
import sys.process._

class MainSpec extends FunSpec {
  describe("valid syntax") {
    List[(String, String)](
      ("1 2 3 + + .", "6"),
      ("5 2 + 10 * .", "70"),
      ("3 4 = .", "0"),
      ("5 5 = .", "-1"),
      ("3 4 < .", "-1"),
      ("3 4 > .", "0"),
      (": say-hello  .\"Hello there!\" ; say-hello", "Hello there!"),
      (": is-it-zero?  0 = if .\"Yes!\" else .\" No!\" then ; 0 is-it-zero?", "Yes!"),
      (": is-it-zero?  0 = if .\"Yes!\" else .\" No!\" then ; 1 is-it-zero?", "No!"),
    ).foreach {
      case (source, result) =>
        describe(source) {
          it(s"should return $result") {
            Main.main(Array[String](source))

            assert(("java Hello" !!).trim == result)
          }
        }
    }
  }
}
