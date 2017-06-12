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

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    var chars: List[Char] = Nil
    var as = 0
    var bs = 0
    var cs = 0
    var timesResult: List[(Char, Int)] = Nil

    def checker(list: List[(Char, Int)]): Int = list match {
      case Nil => 0
      case (c, i) :: xs =>
        if (c == 'a') as = i
        else if (c == 'b') bs = i
        else if (c == 'c') cs = i
        i + checker(xs)
    }

    // Test 1
    chars = 'a' :: 'b' :: 'a' :: Nil
    as = 0
    bs = 0
    cs = 0
    timesResult = times(chars)
    assert(checker(timesResult) == 3)
    assert(as == 2)
    assert(bs == 1)

    // Test 2
    chars = 'a' :: 'a' :: 'a' :: 'a' :: 'b' :: 'a' :: 'c' :: 'b' :: Nil
    as = 0
    bs = 0
    cs = 0
    timesResult = times(chars)
    assert(checker(timesResult) == 8)
    assert(as == 5)
    assert(bs == 2)
    assert(cs == 1)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    assert(!singleton(Nil))
    assert(singleton(Leaf('a', 1) :: Nil))
    assert(!singleton(Leaf('a', 1) :: Leaf('b', 1) :: Nil))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
