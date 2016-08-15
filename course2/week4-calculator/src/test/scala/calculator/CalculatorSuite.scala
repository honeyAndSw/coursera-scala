package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("colorForRemainingCharsCount with Var") {
    val value: Var[Int] = Var(16)
    val resultGreen = TweetLength.colorForRemainingCharsCount(value)
    assert(resultGreen() == "green")

    value.update(14)
    assert(resultGreen() == "orange")
  }

  test("Polynomial.computeDelta") {
    // x^2 + 2x + 1
    val delta1 = Polynomial.computeDelta(Var(1), Var(2), Var(1))
    assert(delta1() == 0)

    // 2x^2 - 4x + 2
    val delta2 = Polynomial.computeDelta(Var(2), Var(-4), Var(2))
    assert(delta2() == 0)

    // x^2 + 3x + 5
    val delta3 = Polynomial.computeDelta(Var(1), Var(3), Var(5))
    assert(delta3() == -11)
  }

  test("Polynomial.computeSolutions") {
    // x^2 + 2x + 1
    val delta1 = Polynomial.computeDelta(Var(1), Var(2), Var(1))
    val solution1 = Polynomial.computeSolutions(Var(1), Var(2), Var(1), delta1)
    val s1: Set[Double] = solution1()
    assert(s1.size == 1)
    assert(s1.equals(Set(-1)))

    val delta2 = Polynomial.computeDelta(Var(1), Var(3), Var(5))
    val solution2 = Polynomial.computeSolutions(Var(1), Var(3), Var(5), delta2)
    assert(solution2().size == 0)
  }

  test("Polynomial.computeSolutions(2)") {
    // x^2 + 4x + 1
    val delta1 = Polynomial.computeDelta(Var(1), Var(4), Var(1))
    val solution1 = Polynomial.computeSolutions(Var(1), Var(4), Var(1), delta1)
    val s1: Set[Double] = solution1()
    assert(s1.size == 2)
  }

  test("Expr.Literal") {
    val exprs: Map[String, Signal[Expr]] = Map("a" -> Signal(new Literal(5)))
    val values = Calculator.computeValues(exprs)

    assert((values("a"))() == 5)
  }

  test("Expr.Refs - cyclic dependencies") {
    // a = b + 1
    val exprA = new Plus(new Ref("b"), new Literal(1))
    // b = 2 * a
    val exprB = new Times(new Literal(2), new Ref("a"))

    val exprs: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(exprA),
      "b" -> Signal(exprB))

    val values = Calculator.computeValues(exprs)

    assert((values("a"))().equals(Double.NaN))
  }
}
