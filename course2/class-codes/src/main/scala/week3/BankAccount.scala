package week3

class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance: Int = balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance = balance + amount
      publish() // Announce to its subscribers that balance has changed.
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      publish() // Announce to its subscribers that balance has changed.
    } else {
      throw new Error("insufficient funds")
    }
}
