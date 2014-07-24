
package wok.csv

import org.specs2.mutable._
import java.io.StringReader


class RowIteratorTest extends SpecificationWithJUnit {

  "RowIterator" should {
    "not close given io.Reader" in {
      val in = new StringReader("")
      new RowIterator(in, Reader()).toList mustEqual Nil
      in.read() mustEqual -1
    }
  }
}