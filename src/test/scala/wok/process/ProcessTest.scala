
package wok.process

import org.specs2.mutable._
import java.nio.charset.StandardCharsets
import java.io.{BufferedOutputStream, PrintStream, ByteArrayOutputStream}


class ProcessTest extends SpecificationWithJUnit {
  "Process.exec" should {
    "redirect stderr output" in {
      val outStream = new ByteArrayOutputStream
      val out = new PrintStream(new BufferedOutputStream(outStream), true, "utf-8")
      Console.withErr(out) {
        new Process(Seq("python", "-c", "import sys; sys.stderr.write('a')"))
          .exec()
      }
      outStream.toByteArray mustEqual "a".getBytes(StandardCharsets.UTF_8)
    }

    "throw IllegalArgumentException when commandStrings is '|'" in {
      new Process(Seq("|"))
        .exec() must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings starts with '|'" in {
      new Process(Seq("|", "a"))
        .exec() must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings ends with '|'" in {
      new Process(Seq("a", "|"))
        .exec() must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings is empty" in {
      new Process(Seq())
        .exec() must throwAn[IllegalArgumentException]
    }

    "call windows programs" in {
      new Process(Seq("cmd", "/c", "echo", "a"))
        .exec() mustEqual "a\r\n".getBytes(StandardCharsets.UTF_8)
    }

    "call cygwin programs" in {
      new Process(Seq("echo", "a"))
        .exec() mustEqual "a\n".getBytes(StandardCharsets.UTF_8)
    }

    "call programs with arguments" in {
      new Process(Seq("cat", "./src/test/scala/wok/process/resources/angel.txt"))
        .exec() mustEqual "angel\r\néindʒəl\r\n".getBytes(StandardCharsets.UTF_8)
    }

    "call programs with unicode encoded arguments" in {
      new Process(Seq("cat", "./src/test/scala/wok/process/resources/angel.txt",
        "|",
        "grep", "éindʒəl"))
        .exec() mustEqual "éindʒəl\n".getBytes(StandardCharsets.UTF_8)
    }

    "call programs with standard input" in {
      new Process(Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-"))
        .exec("stdin-data".getBytes(StandardCharsets.UTF_8)) mustEqual "angel\r\néindʒəl\r\nstdin-data".getBytes(StandardCharsets.UTF_8)
    }

    "call programs with unicode encoded standard input" in {
      new Process(Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-"))
        .exec("éindʒəl".getBytes(StandardCharsets.UTF_8)) mustEqual "angel\r\néindʒəl\r\néindʒəl".getBytes(StandardCharsets.UTF_8)
    }

    "connect programs" in {
      new Process(Seq("cat", "./src/test/scala/wok/process/resources/angel.txt",
        "|",
        "grep", "an"))
        .exec() mustEqual "angel\n".getBytes(StandardCharsets.UTF_8)
    }
  }
}