
package wok.csv

import org.specs2.mutable._


class JointCharSequenceTest extends SpecificationWithJUnit {
  "JointCharSequence.subString" should {
    "return CharSequence when start and end are in bounds" in {
      new JointCharSequence("a", "b")
        .subSequence(0, 0) mustEqual ""
      new JointCharSequence("a", "b")
        .subSequence(0, 1) mustEqual "a"
      new JointCharSequence("a", "b")
        .subSequence(0, 2) mustEqual new JointCharSequence("a", "b")
      new JointCharSequence("a", "b")
        .subSequence(1, 1) mustEqual ""
      new JointCharSequence("a", "b")
        .subSequence(1, 2) mustEqual "b"
    }
    "throw IndexOutOfBoundsException when start greater than end" in {
      new JointCharSequence("a", "b")
        .subSequence(1, 0) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(2, 0) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(2, 1) must throwA[IndexOutOfBoundsException]
    }
    "throw IndexOutOfBoundsException when start or end is out of bounds" in {
      new JointCharSequence("a", "b")
        .subSequence(-1, 0) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(-1, 1) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(-1, 2) must throwA[IndexOutOfBoundsException]

      new JointCharSequence("a", "b")
        .subSequence(0, 3) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(1, 3) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .subSequence(2, 3) must throwA[IndexOutOfBoundsException]
    }
  }

  "JointCharSequence.charAt" should {

    "return Char when index in bounds" in {
      new JointCharSequence("a", "b")
        .charAt(0) mustEqual 'a'
      new JointCharSequence("a", "b")
        .charAt(1) mustEqual 'b'
    }

    "throw IndexOutOfBoundsException when index out of bounds" in {
      new JointCharSequence("a", "b")
        .charAt(-1) must throwA[IndexOutOfBoundsException]
      new JointCharSequence("a", "b")
        .charAt(2) must throwA[IndexOutOfBoundsException]
    }
  }

  "JointCharSequence.length" should {

    "return a.legth + b.length" in {
      new JointCharSequence("a", "b")
        .length mustEqual 2
      new JointCharSequence("ab", "cd")
        .length mustEqual 4
    }
  }
}
