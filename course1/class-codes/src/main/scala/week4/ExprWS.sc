package week4

object exprs {
  val show1 = show(Num(1))
  show(Sum(Num(1), Num(2)))

  trait Expr
  case class Num(n: Int) extends Expr {
    override def toString = n.toString
  }
  case class Sum(e1: Expr, e2: Expr) extends  Expr

  def show(e: Expr): String = e match {
    case Num(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }

}