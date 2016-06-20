package week3

object intsets {
  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 include 4

  class Empty extends IntSet {
    def include(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

    def contains(x: Int): Boolean = false

    override def union(other: IntSet): IntSet = other

    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
    def include(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left include x, right)
      else if (x > elem) new NonEmpty(elem, left, right include x)
      else this

    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def union(other: IntSet): IntSet =
      ((left union right) union other) include elem

    override def toString = "{" + left + elem + right + "}"
  }

  abstract class IntSet {
    def include(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }
}

