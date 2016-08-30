package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceRecursive(chars: Array[Char], open: Int, close: Int): Boolean = {
      if (open < close) {
        return false
      }

      // Base condition
      if (chars.isEmpty) {
        return open == close
      }

      val head = chars.head
      if (head == '(') {
        balanceRecursive(chars.tail, open + 1, close)
      } else if (head == ')') {
        balanceRecursive(chars.tail, open, close + 1)
      } else {
        balanceRecursive(chars.tail, open, close)
      }
    }

    /**
      * Modified balanceRecursive.
      * Use one accumulator instead of two.
      *
      * @param chars
      * @param diff
      * @return
      */
    def balanceRecursive2(chars: Array[Char], diff: Int): Boolean = {
      if (diff < 0) {
        return false
      }

      // Base condition
      if (chars.isEmpty) {
        return diff == 0
      }

      val head = chars.head
      if (head == '(') {
        balanceRecursive2(chars.tail, diff + 1)
      } else if (head == ')') {
        balanceRecursive2(chars.tail, diff - 1)
      } else {
        balanceRecursive2(chars.tail, diff)
      }
    }

    balanceRecursive2(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    type AccPair = (Int, Int)

    def traverse(from: Int, until: Int, leftAcc: Int, rightAcc: Int): AccPair = {
      if (from >= until) {
        (leftAcc, rightAcc)
      } else {
        val ch = chars(from)
        if (ch == '(') {
          traverse(from + 1, until, leftAcc + 1, rightAcc)
        } else if (ch == ')') {
          if (leftAcc > 0) {
            traverse(from + 1, until, leftAcc - 1, rightAcc)
          } else {
            traverse(from + 1, until, leftAcc, rightAcc + 1)
          }
        } else {
          traverse(from + 1, until, leftAcc, rightAcc)
        }
      }
    }

    def reduce(from: Int, until: Int): AccPair = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (from + until) / 2
        val ((lOpen, lClose), (rOpen, rClose)) = parallel[AccPair, AccPair](
          reduce(from, mid),
          reduce(mid, until))

        // Balancing unbalanced parentheses
        val o = lOpen + rOpen
        val c = lClose + rClose
        if (lOpen >= rOpen) (o - c, 0) else (o, c)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
