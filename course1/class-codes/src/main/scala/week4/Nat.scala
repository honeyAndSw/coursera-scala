package week4

/**
  * Nat, peano number class.
  * https://wiki.haskell.org/Peano_numbers
  * Positive number class with simple operations
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def sucessor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) this else throw new Error("negative number.")

  override def predecessor: Nat = throw new Error("Zero doesn't have a predecessor.")
}

/**
  * Successor of n
  * @example new Succ(4) conceptually represents '5'.
  *          new Succ(4).predecessor = 4
  *          new Succ(4).successor = new Succ(new Succ(4)) = 6
  */
class Succ(n: Nat) extends Nat {
  override def isZero = false

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  override def predecessor: Nat = n
}