package wok.core

import org.specs2.mutable._


class CliOptionTest extends SpecificationWithJUnit {

  "CliOption.parse" should {
    "throw AssertionError" in {
      "when parse -v" in {
        CliOption.parse(List("-v")) must throwAn[AssertionError]
      }
      "when parse -v value not containing '='" in {
        CliOption.parse(List("-v", "a")) must throwAn[AssertionError]
      }
      "when parse -v value containing '=', but at last" in {
        CliOption.parse(List("-v", "a=")) must throwAn[AssertionError]
      }
      "when parse -v@" in {
        CliOption.parse(List("-v@")) must throwAn[AssertionError]
      }
      "when parse -v@ value not containing '='" in {
        CliOption.parse(List("-v@", "a")) must throwAn[AssertionError]
      }
      "when parse -v@ value containing '=', but at last" in {
        CliOption.parse(List("-v@", "a=")) must throwAn[AssertionError]
      }
      "when parse -v@non_supported_type value" in {
        CliOption.parse(List("-v@non_supported_type", "a=b")) must throwAn[AssertionError]
      }
      "when parse -b" in {
        CliOption.parse(List("-b")) must throwAn[AssertionError]
      }
      "when parse -e" in {
        CliOption.parse(List("-e")) must throwAn[AssertionError]
      }
    }

    "parse -v a=b" in {
      CliOption.parse(List("-v", "a=b")) mustEqual CliOption(Nil, List("var a = b"), None, Nil, false)
    }
    "parse -v@str a=b" in {
      CliOption.parse(List("-v@str", "a=b")) mustEqual CliOption(Nil, List("var a = \"b\""), None, Nil, false)
    }
    "parse -v@rawstr a=b" in {
      CliOption.parse(List("-v@rawstr", "a=b")) mustEqual CliOption(Nil, List("var a = \"\"\"b\"\"\""), None, Nil, false)
    }
    "parse -v@char a=b" in {
      CliOption.parse(List("-v@char", "a=b")) mustEqual CliOption(Nil, List("var a = 'b'"), None, Nil, false)
    }
    "parse -b a" in {
      CliOption.parse(List("-b", "a")) mustEqual CliOption(Nil, List("a"), None, Nil, false)
    }
    "parse -e a" in {
      CliOption.parse(List("-e", "a")) mustEqual CliOption(Nil, Nil, None, List("a"), false)
    }
    "parse a" in {
      CliOption.parse(List("a")) mustEqual CliOption(Nil, Nil, Some("a"), Nil, false)
    }
    "parse a b" in {
      CliOption.parse(List("a", "b")) mustEqual CliOption(List("b"), Nil, Some("a"), Nil, false)
    }
    "parse --" in {
      CliOption.parse(List("--")) mustEqual CliOption(Nil, Nil, None, Nil, false)
    }
    "parse --diag" in {
      CliOption.parse(List("--diag")) mustEqual CliOption(Nil, Nil, None, Nil, true)
    }
    "parse -- a" in {
      CliOption.parse(List("--", "a")) mustEqual CliOption(List("a"), Nil, None, Nil, false)
    }
  }
}