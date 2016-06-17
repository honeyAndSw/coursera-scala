package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRecursive(chars: List[Char], open: Int, close: Int): Boolean = {
      // Base condition of recursion
      if (chars.isEmpty) {
        return open == close
      }

      if (open < close) {
        return false
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

    balanceRecursive(chars, 0, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeRecursive(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        return 1
      }

      // When money is greater than 0 but no more coins, it's impossible to count change.
      if (coins.isEmpty) {
        return 0
      }

      val coin = coins.head
      val tail = coins.tail

      if (money < coin) {
        return 0
      }

      var count = 0
      var remain = money

      while (remain >= 0) {
        count += countChange(remain, tail)
        remain -= coin
      }

      return count
    }

    countChangeRecursive(money, coins.sortWith(_ < _))
  }
}
