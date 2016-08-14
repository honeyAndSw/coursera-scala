package week3

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _ // Initially uninitialized.
  sum()

  private def sum() = {
    total = observed.map(_.currentBalance).sum
  }

  def handler(pub: Publisher) = sum()

  def totalBalance = {
    println("----- compute total -----")
    total
  }
}
