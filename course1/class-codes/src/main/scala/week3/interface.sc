object intsets {

}

class Empty extends IntSet {
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  override def contains(x: Int): Boolean = false
}

class NonEmpty(x: Int, empty: Empty, empty1: Empty) extends IntSet{
  override def incl(x: Int): IntSet = ???

  override def contains(x: Int): Boolean = ???
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

