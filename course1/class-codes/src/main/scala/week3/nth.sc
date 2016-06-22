package week3

/**
  * Created by naheon on 2016. 6. 22..
  */
object ListNth {
  def nth[T](n: Int, list: List[T]): T = nthRecursive(n, 0, list)

  def nthRecursive[T](n: Int, p: Int, list: List[T]): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException()

    if (n == p) list.head

    nthRecursive(n, p + 1, list.tail)
  }
}
