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

  test("decode and encode") {
    new TestTrees {
      assert(decode(t1, encode(t1)("aaabaaabaaaabbbb".toList)) === "aaabaaabaaaabbbb".toList)
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("aaabaaabaaaabbbb".toList)) === "aaabaaabaaaabbbb".toList)
    }
  }

  test("misc 1") {
    val str = "abcdefg"
    assert(decode(frenchCode, quickEncode(frenchCode)(str.toList)) === str.toList)
  }

  // -----------------------------------
  //
  // Real tests
  //
  // -----------------------------------

  trait TestHuffman{
    val code = "aaaaaaaabbbcdefgh".toList

    val tree = makeCodeTree( // abcdefgh
      Leaf('a', 8),
      makeCodeTree( // bcd
        makeCodeTree(
          Leaf('b', 3),
          makeCodeTree( // cd
            Leaf('c', 1),
            Leaf('d', 1)
          )
        ),
        makeCodeTree( // efgh
          makeCodeTree( // ef
            Leaf('e', 1),
            Leaf('f', 1)
          ),
          makeCodeTree( // gh
            Leaf('g', 1),
            Leaf('h', 1)
          )
        )
      )
    )

    def evalTimes = times(code)
    def evalMakeOrderedLeafList = makeOrderedLeafList(evalTimes)

    val codeTable = List(
      ('a', List(0)),
      ('b', List(1, 0, 0)),
      ('c', List(1, 0, 1, 0)),
      ('d', List(1, 0, 1, 1)),
      ('e', List(1, 1, 0, 0)),
      ('f', List(1, 1, 0, 1)),
      ('g', List(1, 1, 1, 0)),
      ('h', List(1, 1, 1, 1))
    )
  }

  test("Test function [times]") {
    new TestHuffman {
      val weights = evalTimes
      assert(weights.length == 8)
      assert(weights.contains(('a', 8)))
      assert(weights.contains(('b', 3)))
      assert(weights.contains(('c', 1)))
      assert(weights.contains(('d', 1)))
      assert(weights.contains(('e', 1)))
      assert(weights.contains(('f', 1)))
      assert(weights.contains(('g', 1)))
      assert(weights.contains(('h', 1)))
    }
  }

  test("Test function [makeOrderedLeafList]") {
    new TestHuffman {
      val leaves = evalMakeOrderedLeafList
      assert(leaves.length == 8)
      val freqs = List(1, 1, 1, 1, 1, 1, 3, 8)

      def check(leaves: List[Leaf], freqs: List[Int]): Unit = leaves match {
        case Nil => assert(freqs.isEmpty)
        case x :: xs =>
          assert(x.weight == freqs.head)
          check(xs, freqs.tail)
      }
    }
  }

  test("Test function [singleton]") {
    assert(!singleton(Nil))
    assert(singleton(Leaf('a', 1) :: Nil))
    assert(!singleton(Leaf('a', 1) :: Leaf('b', 1) :: Nil))
  }

  test("Test function [combine]") {
    new TestHuffman {
      val combined1 = combine(makeOrderedLeafList(evalTimes))
      assert(combined1 === List(
        Leaf('f', 1),
        Leaf('e', 1),
        Leaf('d', 1),
        Leaf('c', 1),
        makeCodeTree(
          Leaf('h', 1),
          Leaf('g', 1)
        ),
        Leaf('b', 3),
        Leaf('a', 8)
      )
      )
    }
  }

  // TODO: Hard to test

  //  test("Test function [createCodeTree]") {
  //    new TestHuffman {
  //      val createdTree = createCodeTree(code)
  //      assert(tree === createdTree)
  //    }
  //  }

  test("Test function [decode] and [encode]") {
    new TestHuffman {
      assert(decode(tree, encode(tree)(code)) === code)
    }
  }

  test("Test function [codeBits]") {
    new TestHuffman {
      val currying = codeBits(codeTable)_
      assert(currying('a') == List(0))
      assert(currying('b') == List(1, 0, 0))
      assert(currying('c') == List(1, 0, 1, 0))
      assert(currying('d') == List(1, 0, 1, 1))
      assert(currying('e') == List(1, 1, 0, 0))
      assert(currying('f') == List(1, 1, 0, 1))
      assert(currying('g') == List(1, 1, 1, 0))
      assert(currying('h') == List(1, 1, 1, 1))
    }
  }

  test("Test function [convert]") {
    new TestHuffman {
      assert(convert(tree) == codeTable)
    }
  }

  test("Test function [quickEncode]") {
    new TestHuffman {
      assert(decode(tree, quickEncode(tree)(code)) === code)
    }
  }
}
