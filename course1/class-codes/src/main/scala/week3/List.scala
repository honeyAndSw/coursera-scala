package week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  override def toString(): String = {
    if (isEmpty) "Nil"
    else head + " - " + tail.toString()
  }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def tail: Nothing = throw new NoSuchElementException("Nil.head")

  override def head: Nothing = throw new NoSuchElementException("Nil.head")
}
