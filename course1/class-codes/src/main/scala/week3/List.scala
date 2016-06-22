package week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List {
  override def isEmpty: Boolean = true

  override def tail: Nothing = throw new NoSuchElementException("Nil.head")

  override def head: Nothing = throw new NoSuchElementException("Nil.head")
}
