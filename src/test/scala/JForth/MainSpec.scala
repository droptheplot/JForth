package JForth

import org.scalatest.FunSpec
import sys.process._

class MainSpec extends FunSpec {
  describe("valid syntax") {
    List[(String, String)](
      ("1 2 3 dup .", "3"),
      ("1 2 3 pop .", "2"),
      ("1 2 3 4 swap .", "3"),
      ("5 5 mod .", "0"),
      ("3 4 < invert .", "0"),
      ("1 2 3 + + .", "6"),
      ("5 2 + 10 * .", "70"),
      ("3 4 = .", "0"),
      ("5 5 = .", "-1"),
      ("3 4 < .", "-1"),
      ("3 4 > .", "0"),
      (": say-hello  .\"Hello there!\" ; say-hello", "Hello there!"),
      (": buzz?  5 mod 0 = if .\" Buzz\" then ; 3 buzz?", ""),
      (": buzz?  5 mod 0 = if .\" Buzz\" then ; 5 buzz?", "Buzz"),
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
