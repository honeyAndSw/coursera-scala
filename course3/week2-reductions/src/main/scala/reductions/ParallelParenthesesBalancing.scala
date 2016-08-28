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

    def traverse(from: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      ???
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      ???
    }

    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
