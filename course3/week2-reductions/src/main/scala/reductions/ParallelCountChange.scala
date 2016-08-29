package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) {
      // Consumes all of the money
      1
    } else if (money < 0 || coins.isEmpty) {
      // No possible ways
      0
    } else {
      // money > 0 and coins is not empty
      val remainCoin = countChange(money - coins.head, coins)
      val dropCoin = countChange(money, coins.tail)
      remainCoin + dropCoin
    }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money < 0 || coins.isEmpty) {
      0
    } else if (threshold(money, coins)) {
      countChange(money, coins)
    } else {
      println(s"invoke parallel $money")
      val (remainCoin, dropCoin) = parallel[Int, Int] (
        parCountChange(money - coins.head, coins, threshold),
        parCountChange(money, coins.tail, threshold)
      )
      remainCoin + dropCoin
    }
  }

  /** Threshold heuristic based on the starting money.
    * Returns true when the amount of money is less than
    * or equal to 2 / 3 of the starting amount. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (money: Int, coins: List[Int]) => money * 3 <= startingMoney * 2

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (money: Int, coins: List[Int]) => coins.size * 3 <= totalCoins * 2

  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold =
    (money: Int, coins: List[Int]) =>
      money * coins.size * 2 <= startingMoney * allCoins.size

}
