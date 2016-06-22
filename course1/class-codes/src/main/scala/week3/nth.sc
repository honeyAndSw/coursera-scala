import week3.List
import week3.Cons
import week3.Nil
/**
  * Created by naheon on 2016. 6. 22..
  */
object ListNth {
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)));
  //nth(0, list)
  //nth(4, list)
  nth(-1, list)

  def nth[T](n: Int, list: List[T]): T = nthRecursive(n, 0, list)

  def nthRecursive[T](n: Int, p: Int, list: List[T]): T = {
    if (n < 0 || list.isEmpty) throw new IndexOutOfBoundsException()

    if (n == p) list.head
    else nthRecursive(n, p + 1, list.tail)
  }
}
