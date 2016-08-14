import week3.{Consolidator, BankAccount}
object Bank {
  val account1 = new BankAccount
  val account2 = new BankAccount
  val consolidator = new Consolidator(List(account1, account2))
  account1 deposit 30
  account2 deposit 10
  consolidator.totalBalance
}