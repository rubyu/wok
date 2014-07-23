
package wok.csv

import org.specs2.mutable._


class QuoteTest extends SpecificationWithJUnit {
  "Quote" should {
    "support access without parens" in {
      Quote.All mustEqual Quote().All()
      Quote.Min mustEqual Quote().Min()
      Quote.None mustEqual Quote().None()
      Quote.E('a') mustEqual Quote().E('a')
      Quote.Q('a') mustEqual Quote().Q('a')
    }

    "be created" in {
      Quote() mustEqual Quote()
    }

    "be created with 'new'" in {
      new Quote() mustEqual Quote()
    }

    "have default value" in {
      Quote().M mustEqual Quote.Mode.Min
      Quote().Q mustEqual Some('"')
      Quote().E mustEqual None
    }
  }
}