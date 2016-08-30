package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("balance should work for string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance[$input] should be $expected")

    def check2(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 4) == expected,
        s"parBalance[$input] should be $expected")

    check("(if (zero? x) max (/ 1 x))", true)
    check("I told him (that it's not (yet) done). (But he wasn't listening)", true)
    check("(o_()", false)
    check(":-)", false)
    check("())(", false)

    check2("(if (zero? x) max (/ 1 x))", true)
    check2("I told him (that it's not (yet) done). (But he wasn't listening)", true)
    check2("(o_()", false)
    check2(":-)", false)
    check2("())(", false)
  }

  test("parBalance") {
    assert(parBalance(")(".toArray, 1) == false)
  }

}