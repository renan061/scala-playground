package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    *   val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val s12 = union(s1, s2)
    val s13 = union(s1, s3)
    val s23 = union(s2, s3)
    val s123 = union(s12, s3)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {
    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("test singletonSet") {
    new TestSets {
      assert(contains(s1, 1), "s1 ok")
      assert(contains(s2, 2), "s2 ok")
      assert(contains(s3, 3), "s3 ok")

      assert(!contains(s1, 2))
      assert(!contains(s2, 3))
      assert(!contains(s3, 4))
    }
  }

  test("test union") {
    new TestSets {
      assert(contains(s12, 1))
      assert(contains(s12, 2))
      assert(!contains(s12, 3))

      assert(contains(s13, 1))
      assert(contains(s13, 3))
      assert(!contains(s13, 2))

      assert(contains(s23, 2))
      assert(contains(s23, 3))
      assert(!contains(s23, 1))

      assert(contains(s123, 1))
      assert(contains(s123, 2))
      assert(contains(s123, 3))
      assert(!contains(s123, 0))
      assert(!contains(s123, 4))
    }
  }

  test("test intersect") {
    new TestSets {
      val _s12 = intersect(s123, s12)
      val _s13 = intersect(s123, s13)
      val _s23 = intersect(s123, s23)
      val _s123 = intersect(s123, union(s123, singletonSet(4)))

      assert(contains(_s12, 1))
      assert(contains(_s12, 2))
      assert(!contains(_s12, 3))

      assert(contains(_s13, 1))
      assert(contains(_s13, 3))
      assert(!contains(_s13, 2))

      assert(contains(_s23, 2))
      assert(contains(_s23, 3))
      assert(!contains(_s23, 1))

      assert(contains(_s123, 1))
      assert(contains(_s123, 2))
      assert(contains(_s123, 3))
      assert(!contains(_s123, 0))
      assert(!contains(_s123, 4))
    }
  }

  test("test diff") {
    new TestSets {
      val _s12 = diff(s123, s3)
      val _s13 = diff(s123, s2)
      val _s23 = diff(s123, s1)
      val _s123 = diff(s123, singletonSet(4))

      assert(contains(_s12, 1))
      assert(contains(_s12, 2))
      assert(!contains(_s12, 3))

      assert(contains(_s13, 1))
      assert(contains(_s13, 3))
      assert(!contains(_s13, 2))

      assert(contains(_s23, 2))
      assert(contains(_s23, 3))
      assert(!contains(_s23, 1))

      assert(contains(_s123, 1))
      assert(contains(_s123, 2))
      assert(contains(_s123, 3))
      assert(!contains(_s123, 0))
      assert(!contains(_s123, 4))
    }
  }

  test("test set filter") {
    new TestSets {
      val _s12 = filter(s123, x => x < 3)
      val _s13 = filter(s123, x => x < 2 || x > 2)
      val _s23 = filter(s123, x => x > 1)
      val _s123 = filter(s123, x => x != 4)

      assert(contains(_s12, 1))
      assert(contains(_s12, 2))
      assert(!contains(_s12, 3))

      assert(contains(_s13, 1))
      assert(contains(_s13, 3))
      assert(!contains(_s13, 2))

      assert(contains(_s23, 2))
      assert(contains(_s23, 3))
      assert(!contains(_s23, 1))

      assert(contains(_s123, 1))
      assert(contains(_s123, 2))
      assert(contains(_s123, 3))
      assert(!contains(_s123, 0))
      assert(!contains(_s123, 4))
    }
  }

  test("test for all") {
    new TestSets {
      assert(forall(s123, x => x > 0 || x < 4))
      assert(forall(s123, x => x < 4))
    }
  }

  test("test exists") {
    new TestSets {
      assert(exists(s123, x => x > 0))
      assert(exists(s123, x => x > 1))
      assert(exists(s123, x => x > 2))
      assert(!exists(s123, x => x > 3))
    }
  }

  test("test map") {
    new TestSets {
      val s234 = map(s123, x => x + 1)
      assert(!contains(s234, 1))
      assert(contains(s234, 2))
      assert(contains(s234, 3))
      assert(contains(s234, 4))
      assert(!contains(s234, 5))
    }
  }
}
