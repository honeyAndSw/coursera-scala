case class Storage (resProd: Double) {
}

object Storage {
  def cnts(prodBig:Int, prodPcs:Int): Storage = {
    val prod = prodPcs / (prodBig + 0.0)
    new Storage(prod)
  }
}

case class Product(prodId:Int, prodBig:Int, prodPcs:Int,
                   price:Int, resProd:Double)

object Product {
  def add(prodId: Int, prodBig: Int, prodPcs: Int): Product = {

    val prod = Storage.cnts(prodBig, prodPcs).resProd
    val price = 4000

    val resJsn = Product(123/*clientId*/, prodBig, prodPcs, price, prod)
    ???
  }
}

