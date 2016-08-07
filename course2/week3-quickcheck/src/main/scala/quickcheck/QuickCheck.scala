package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable.IndexedSeq

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k1 <- arbitrary[Int]
    k2 <- arbitrary[Int]
    h1 <- insert(k1, empty)
    h2 <- insert(k2, h1)
  } yield {
    h2
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("insert min, find min back") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * Hint1
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get the smallest of the two elements back.
    */
  property("insert two ints to empty heap, find min") = forAll { (a: Int, b: Int) =>
    val twoElemHeap = insert(b, insert(a, empty))
    findMin(twoElemHeap) == Math.min(a, b)
  }

  /**
    * Hint2
    * If you insert an element into an empty heap,
    * then delete the minimum, the resulting heap should be empty.
    */
  property("insert and delete") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  /**
    * Hint3
    * Given any heap, you should get a sorted sequence of elements
    * when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("continuous deleteMin and sorting") = forAll { h: H =>
    val sorted: List[A] = deleteAndSort(h)
    isSorted(sorted, ord)
  }

  def deleteAndSort(heap: H): List[A] = {
    if (isEmpty(heap)) Nil
    else findMin(heap) :: deleteAndSort(deleteMin(heap))
  }

  def isSorted(list: List[A], order: Ordering[A]): Boolean = list match {
    case Nil => true
    case head::Nil => true
    case head::second::tail => {
      if (order.lteq(head, second)) isSorted(tail, order)
      else false
    }
  }

  /**
    * Hint4
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other.
    */
  property("find min of a heap that two heap are melt") = forAll { (h1: H, h2: H) =>
    val expectedMin =
      if (isEmpty(h1) && isEmpty(h2)) 0
      else if (isEmpty(h1)) findMin(h2)
      else if (isEmpty(h2)) findMin(h1)
      else Math.min(findMin(h1), findMin(h2))

    val melding = meld(h1, h2)
    val meldingMin = if (isEmpty(melding)) 0 else findMin(melding)
    meldingMin == expectedMin
  }
}
