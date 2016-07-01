package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}

  test("Basics: weight and chars") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times") {
    new TestTrees {
      val result: List[(Char, Int)] = times(List('a','b'))

      result(0) match {
        case (theChar, theInt) =>
          assert(theChar == 'a')
          assert(theInt == 1)
      }

      result(1) match {
        case (theChar, theInt) =>
          assert(theChar == 'b')
          assert(theInt == 1)
      }
    }
  }

  test("times : has duplicated chars") {
    new TestTrees {
      val result: List[(Char, Int)] = times(List('a', 'b', 'a'))

      assert(result.size == 2)

      result(0) match {
        case (theChar, theInt) =>
          assert(theChar == 'a')
          assert(theInt == 2)
      }

      result(1) match {
        case (theChar, theInt) =>
          assert(theChar == 'b')
          assert(theInt == 1)
      }
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
        === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))

    val l2 = List(('t', 1), ('e', 1), ('x', 3))
    assert(makeOrderedLeafList(l2) === List(Leaf('t',1), Leaf('e',1), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))

    val leaflist2 = List(Leaf('e', 1), Leaf('t', 5), Leaf('x', 4))
    assert(combine(leaflist2) === List(Leaf('x',4), Fork(Leaf('e',1),Leaf('t',5),List('e', 't'),6)))
  }

  test("createCodeTree") {
    createCodeTree(string2Chars("ABCDE")) match {
      case f: Fork =>
        assert(f.weight == 5)
    }

    createCodeTree(string2Chars("AABCD")) match {
      case f: Fork =>
        assert(f.weight == 5)
    }
  }

  test("Decode secretCode") {
    val code = decodedSecret
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val encoded = encode(t1)("ab".toList)
      println(encoded)
      assert(decode(t1, encoded) === "ab".toList)
    }
  }

  test("encode and quick encode make same results") {
    new TestTrees {
      val encoded = encode(t1)("ab".toList)
      val quickEncoded = quickEncode(t1)("ab".toList)
      assert(encoded.equals(quickEncoded))
    }

    new TestTrees {
      assert(encode(t2)("bad".toList) equals encode(t2)("bad".toList))
    }
  }

}
