package wok.core

import org.specs2.mutable._

import scalax.file.Path


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
      CliOption.parse(List("-v", "a=b")) mustEqual CliOption(Nil, List("var a = b"), Nil, Nil, false)
    }
    "parse -v@str a=b" in {
      CliOption.parse(List("-v@str", "a=b")) mustEqual CliOption(Nil, List("var a = \"b\""), Nil, Nil, false)
    }
    "parse -v@rawstr a=b" in {
      CliOption.parse(List("-v@rawstr", "a=b")) mustEqual CliOption(Nil, List("var a = \"\"\"b\"\"\""), Nil, Nil, false)
    }
    "parse -v@char a=b" in {
      CliOption.parse(List("-v@char", "a=b")) mustEqual CliOption(Nil, List("var a = 'b'"), Nil, Nil, false)
    }
    "parse -b a" in {
      CliOption.parse(List("-b", "a")) mustEqual CliOption(Nil, List("a"), Nil, Nil, false)
    }
    "parse -e a" in {
      CliOption.parse(List("-e", "a")) mustEqual CliOption(Nil, Nil, Nil, List("a"), false)
    }
    "parse a" in {
      CliOption.parse(List("a")) mustEqual CliOption(Nil, Nil, List("a"), Nil, false)
    }
    "parse a b" in {
      CliOption.parse(List("a", "b")) mustEqual CliOption(List("b"), Nil, List("a"), Nil, false)
    }
    "parse --" in {
      CliOption.parse(List("--")) mustEqual CliOption(Nil, Nil, Nil, Nil, false)
    }
    "parse --diag" in {
      CliOption.parse(List("--diag")) mustEqual CliOption(Nil, Nil, Nil, Nil, true)
    }
    "parse -f" in {
      "parse -b" in {
        val temp = Path.createTempFile()
        temp.write(List("-b foo", "-b bar", "-b baz") mkString "\n")
        CliOption.parse(List("-f", temp.path)) mustEqual CliOption(Nil, List("foo", "bar", "baz"), Nil, Nil, false)
      }
      "parse -e" in {
        val temp = Path.createTempFile()
        temp.write(List("-e foo", "-e bar", "-e baz") mkString "\n")
        CliOption.parse(List("-f", temp.path)) mustEqual CliOption(Nil, Nil, Nil, List("foo", "bar", "baz"), false)
      }
      "parse scripts" in {
        val temp = Path.createTempFile()
        temp.write(List("foo", "bar", "baz") mkString "\n")
        CliOption.parse(List("-f", temp.path)) mustEqual CliOption(Nil, Nil, List("foo", "bar", "baz"), Nil, false)
      }
      "parse all type" in {
        val temp = Path.createTempFile()
        temp.write(List("-b b1", "-b b2", "-b b3", "s1", "s2", "s3", "-e e1", "-e e2", "-e e3") mkString "\n")
        CliOption.parse(List("-f", temp.path)) mustEqual
          CliOption(Nil, List("b1", "b2", "b3"), List("s1", "s2", "s3"), List("e1", "e2", "e3"), false)
      }
      "marge with -b" in {
        val temp = Path.createTempFile()
        temp.write(List("-b b2") mkString "\n")
        CliOption.parse(List("-b", "b1", "-f", temp.path)) mustEqual
          CliOption(Nil, List("b1", "b2"), Nil, Nil, false)
      }
      "marge with -e" in {
        val temp = Path.createTempFile()
        temp.write(List("-e e2") mkString "\n")
        CliOption.parse(List("-e", "e1", "-f", temp.path)) mustEqual
          CliOption(Nil, Nil, Nil, List("e1", "e2"), false)
      }
    }
    "parse -- a" in {
      CliOption.parse(List("--", "a")) mustEqual CliOption(List("a"), Nil, Nil, Nil, false)
    }
  }
}