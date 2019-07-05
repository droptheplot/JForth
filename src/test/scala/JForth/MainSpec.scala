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
      (": loop-test  10 0 do i . loop ; loop-test", "0123456789"),
      (": fizz?  3 mod 0 = dup if .\" Fizz\" then ; : buzz?  5 mod 0 = dup if .\" Buzz\" then ; : fizz-buzz?  dup fizz? swap buzz? or invert ; : do-fizz-buzz 25 1 do i fizz-buzz? if i . then loop ; do-fizz-buzz",
       "12Fizz4BuzzFizz78FizzBuzz11Fizz1314FizzBuzz1617Fizz19BuzzFizz2223Fizz"),
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
