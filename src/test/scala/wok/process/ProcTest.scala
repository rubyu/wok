
package scala.sys.patched.process {
  import java.io._
  import java.lang.reflect.InvocationTargetException

  object debug {

    class ProcessMock(error: Boolean) extends Process {
      var destroyCount = 0
      def exitValue(): Int = {
        if (error) {
          throw new InterruptedException()
        }
        0
      }
      def destroy(): Unit = { destroyCount += 1 }
    }

    class ProcessBuilderMock(process: Process, error: Boolean) extends ProcessBuilder.AbstractBuilder {
      override def run(io: ProcessIO): Process = {
        if (error) {
          throw new IOException()
        }
        process
      }
    }

    class PipeSinkMock extends Process.PipeSink("PipeSinkMock") {
      var releaseCount = 0
      override val pipe = null
      override val sink = null
      override def run(): Unit = {}
      override def connectOut(out: OutputStream): Unit = {}
      override def connectIn(pipeOut: PipedOutputStream): Unit = {}
      override def release(): Unit = { releaseCount += 1 }
    }

    class PipeSourceMock extends Process.PipeSource("PipeSourceMock") {
      var releaseCount = 0
      override val pipe = null
      override val source = null
      override def run(): Unit = {}
      override def connectIn(in: InputStream): Unit = {}
      override def connectOut(sink: Process.PipeSink): Unit = {}
      override def release(): Unit = { releaseCount += 1 }
    }

    class PipedProcesses(a: ProcessBuilder, b: ProcessBuilder, defaultIO: ProcessIO, toError: Boolean)
      extends Process.PipedProcesses(a, b, defaultIO, toError) {
      def callRunAndExitValue(source: Process.PipeSource, sink: Process.PipeSink) = {
        val m = classOf[Process.PipedProcesses].getDeclaredMethod("runAndExitValue", classOf[Process.PipeSource], classOf[Process.PipeSink])
        m.setAccessible(true)
        try m.invoke(this, source, sink).asInstanceOf[Option[Int]]
        catch {
          case err: InvocationTargetException => throw err.getTargetException
        }
      }
    }

    def throwsIOException(f: => Unit) = {
      try { f; false }
      catch { case _: IOException => true }
    }

    class PipeSink extends Process.PipeSink("TestPipeSink") {
      def ensureRunloopStarted() = {
        while (sink.size() > 0) {
          Thread.sleep(1)
        }
      }
      def isReleased = {
        val field = classOf[Process.PipeSink].getDeclaredField("pipe")
        field.setAccessible(true)
        val pipe = field.get(this).asInstanceOf[PipedInputStream]
        !this.isAlive && throwsIOException { pipe.read() }
      }
    }

    class PipeSource extends Process.PipeSource("TestPipeSource") {
      def ensureRunloopStarted() = {
        while (source.size() > 0) {
          Thread.sleep(1)
        }
      }
      def isReleased = {
        val field = classOf[Process.PipeSource].getDeclaredField("pipe")
        field.setAccessible(true)
        val pipe = field.get(this).asInstanceOf[PipedOutputStream]
        !this.isAlive && throwsIOException { pipe.write(1) }
      }
    }
  }
}

import org.specs2.mutable._


class ProcTest extends SpecificationWithJUnit {

  "by reflection" in {
    import scala.sys.patched.process.debug._
    import scala.sys.patched.process.{ProcessLogger, BasicIO}
    import java.io.{Closeable, ByteArrayInputStream, ByteArrayOutputStream, InputStream, IOException}
    import scala.concurrent.{Future, Await}
    import scala.concurrent.duration.{Duration, SECONDS}
    import scala.util.control.Exception._


    "PipedProcesses need not to release resources when it normally ends" in {
      val io = BasicIO(false, ProcessLogger(_ => ()))
      val source = new PipeSourceMock
      val sink = new PipeSinkMock
      val a = new ProcessMock(error = false)
      val b = new ProcessMock(error = false)
      val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
      val f = Future {
        p.callRunAndExitValue(source, sink)
      }
      Await.result(f, Duration(5, SECONDS))
      source.releaseCount mustEqual 0
      sink.releaseCount mustEqual 0
      a.destroyCount mustEqual 0
      b.destroyCount mustEqual 0
    }

    "PipedProcesses must release resources when b.run() fails" in {
      val io = BasicIO(false, ProcessLogger(_ => ()))
      val source = new PipeSourceMock
      val sink = new PipeSinkMock
      val a = new ProcessMock(error = false)
      val b = new ProcessMock(error = false)
      val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = true), io, false)
      val f = Future {
        ignoring(classOf[IOException]) {
          p.callRunAndExitValue(source, sink)
        }
      }
      Await.result(f, Duration(5, SECONDS))
      source.releaseCount mustEqual 1
      sink.releaseCount mustEqual 1
      a.destroyCount mustEqual 0
      b.destroyCount mustEqual 0
    }

    "PipedProcesses must release resources when a.run() fails" in {
      val io = BasicIO(false, ProcessLogger(_ => ()))
      val source = new PipeSourceMock
      val sink = new PipeSinkMock
      val a = new ProcessMock(error = false)
      val b = new ProcessMock(error = false)
      val p = new PipedProcesses(new ProcessBuilderMock(a, error = true), new ProcessBuilderMock(b, error = false), io, false)
      val f = Future {
        ignoring(classOf[IOException]) {
          p.callRunAndExitValue(source, sink)
        }
      }
      Await.result(f, Duration(5, SECONDS))
      source.releaseCount mustEqual 1
      sink.releaseCount mustEqual 1
      a.destroyCount mustEqual 0
      b.destroyCount mustEqual 1
    }

    "PipedProcesses must release resources when interrupted waiting for first.exitValue()" in {
      val io = BasicIO(false, ProcessLogger(_ => ()))
      val source = new PipeSourceMock
      val sink = new PipeSinkMock
      val a = new ProcessMock(error = true)
      val b = new ProcessMock(error = false)
      val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
      val f = Future {
        p.callRunAndExitValue(source, sink)
      }
      Await.result(f, Duration(5, SECONDS))
      source.releaseCount mustEqual 1
      sink.releaseCount mustEqual 1
      a.destroyCount mustEqual 1
      b.destroyCount mustEqual 1
    }

    "PipedProcesses must release resources when interrupted waiting for second.exitValue()" in {
      val io = BasicIO(false, ProcessLogger(_ => ()))
      val source = new PipeSourceMock
      val sink = new PipeSinkMock
      val a = new ProcessMock(error = false)
      val b = new ProcessMock(error = true)
      val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
      val f = Future {
        p.callRunAndExitValue(source, sink)
      }
      Await.result(f, Duration(5, SECONDS))
      source.releaseCount mustEqual 1
      sink.releaseCount mustEqual 1
      a.destroyCount mustEqual 1
      b.destroyCount mustEqual 1
    }

    trait CloseChecking extends Closeable {
      var closed = false
      override def close() = closed = true
    }
    class DebugOutputStream extends ByteArrayOutputStream with CloseChecking
    class DebugInputStream(s: String) extends ByteArrayInputStream(s.getBytes()) with CloseChecking
    class DebugInfinityInputStream extends InputStream with CloseChecking {
      def read() = 1
    }

    def sourceSink() = {
      val source = new PipeSource
      val sink = new PipeSink
      source connectOut sink
      source.start()
      sink.start()
      (source, sink)
    }

    "PipeSource and PipeSink must release resources when it normally ends" in {
      val in = new DebugInputStream("aaa")
      val (source, sink) = sourceSink()
      val out = new DebugOutputStream
      source connectIn in
      sink connectOut out
      val f = Future {
        source.join()
        sink.join()
      }
      Await.result(f, Duration(5, SECONDS))
      in.closed mustEqual true
      out.closed mustEqual true
      source.isReleased mustEqual true
      sink.isReleased mustEqual true
    }

    "PipeSource and PipeSink must release resources when when waiting for source.take()" in {
      val (source, sink) = sourceSink()
      val out = new DebugOutputStream
      sink connectOut out
      val f = Future {
        sink.ensureRunloopStarted()
        source.release()
        sink.release()
      }
      Await.result(f, Duration(5, SECONDS))
      out.closed mustEqual true
      source.isReleased mustEqual true
      sink.isReleased mustEqual true
    }

    "PipeSource and PipeSink must release resources when when waiting for sink.take()" in {
      val in = new DebugInputStream("aaa")
      val (source, sink) = sourceSink()
      source connectIn in
      val f = Future {
        source.ensureRunloopStarted()
        source.release()
        sink.release()
      }
      Await.result(f, Duration(5, SECONDS))
      in.closed mustEqual true
      source.isReleased mustEqual true
      sink.isReleased mustEqual true
    }

    "PipeSource and PipeSink must release resources when copying stream" in {
      val in = new DebugInfinityInputStream
      val (source, sink) = sourceSink()
      val out = new DebugOutputStream
      source connectIn in
      sink connectOut out
      val f = Future {
        source.ensureRunloopStarted()
        sink.ensureRunloopStarted()
        source.release()
        sink.release()
      }
      Await.result(f, Duration(5, SECONDS))
      in.closed mustEqual true
      out.closed mustEqual true
      source.isReleased mustEqual true
      sink.isReleased mustEqual true
    }

    /*
  "PipedProcesses.runAndExitValue() x10 return value within a second" in {
    import scala.sys.patched.process.{Process, ProcessLogger}
    import scala.concurrent.{Future, Await}
    import scala.concurrent.duration.Duration
    import scala.concurrent.duration.SECONDS
    import java.io.ByteArrayInputStream

    def input = new ByteArrayInputStream("a".getBytes())

    val f = Future {
      for (i <- 0 until 10) {
        (Process("wc") #< input).run(ProcessLogger { _ => () }).exitValue()
      }
    }
    Await.result(f, Duration(5, SECONDS))
    success
  }
  */

    /*
  "Proc.escape" should {
    def python(s: String) =
      new Proc(Seq("python", "-c", "\"import sys; sys.stdout.write(sys.argv[1])\"", s)).exec().string

    def echo(s: String) =
      new Proc(Seq("echo", s)).exec().string.dropRight(1)

    val patterns = Seq(
      "",
      "a",  //normal character
      " ", "\t",  //spaces
      "%DUMMY%", "%PATH%",   //windows environment parameters
      "\\", "\"", "'", "^", "<", ">", "|", "[", "]", "&", "%" //windows cmd.exe's special characters
    )

    "escape given parameters" in {
      patterns map { p =>
        python(Proc.escape(p)) mustEqual p
        echo(Proc.escape(p)) mustEqual p
      }
    }
  }

  "Proc.exec" should {
    "throw IllegalArgumentException when commandStrings is '|'" in {
      Seq("|")
        .exec() must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings starts with '|'" in {
      Seq("|", "a")
        .exec() must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings ends with '|'" in {
      Seq("a", "|")
        .exec() must throwAn[IllegalArgumentException]
    }

    "throw IllegalArgumentException when commandStrings is empty" in {
      Seq()
        .exec() must throwAn[IllegalArgumentException]
    }

    /*
    "process 1 pass" in {
      import scala.sys.process._
      val input = Patched.IStreamBuilder(new ByteArrayInputStream(("1").getBytes))
      0 to 10 foreach { i =>
        (new Patched.PipedBuilder(input, "wc", false)).!
        println(s"p1p OK $i")
      }
      success
    }

    "process 2 pass" in {
      import scala.sys.process._
      0 to 10 foreach { i =>
        ("echo 1" #| "find /v \"\"").!
        println(s"p2p OK $i")
      }
      success
    }
    */

    "process 2 pass" in {
      import scala.sys.process._
      import scala.concurrent.Future

      val input = Patched.IStreamBuilder(new ByteArrayInputStream(("1").getBytes))
      def command =
        if (System.getProperty("os.name", "") startsWith "Windows")
          new Patched.PipedBuilder(input, "find /v \"\"", false)
        else
          new Patched.PipedBuilder(input, "wc", false)
      val f = Future {
        for (i <- 0 until 10) {
          println(s"p2p $i")
          command.run(ProcessLogger { _ => () }).exitValue()
        }
      }
      Thread.sleep(1000)
      //println(f.isCompleted)
      f.isCompleted must beTrue
    }

    /* x .check
true
     */

    /* x .scala
import scala.sys.process._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.ByteArrayInputStream

object Test {
  def main(args: Array[String]) {
    def input = new ByteArrayInputStream(("a").getBytes())
    def process =
      if (System.getProperty("os.name", "") startsWith "Windows")
        "find /v \"\"" #< input
      else
        "wc" #< input
    val f = Future {
      for (i <- 0 until 10) {
        process.run(ProcessLogger { _ => () }).exitValue()
      }
    }
    Thread.sleep(1000)
    println(f.isCompleted)
  }
}
     */


    "not found" in {
      import scala.sys.process._
      "aa".run().exitValue() must throwA[InterruptedException]
    }

    "process 2 fail" in {
      import scala.sys.process._
      import scala.concurrent.Future

      def input = new ByteArrayInputStream(("1").getBytes)
      def command =
        if (System.getProperty("os.name", "") startsWith "Windows")
          "find /v \"\"" #< input
        else
          "wc" #< input
      val f = Future {
        for (i <- 0 until 10) {
          println(s"p2f $i")
          command.run(ProcessLogger { _ => () }).exitValue()
        }
      }
      Thread.sleep(1000)
      //println(f.isCompleted)
      f.isCompleted must beTrue
    }

    /*
    "process 1" in {
      import scala.sys.process._
      val input = new ByteArrayInputStream(("1" * 10000).getBytes)
      0 to 10000 foreach { i =>
        ("wc" #< input).!
        println(s"p1 OK $i")
      }
      success
    }
    */

    /*
    "process 2" in {
      0 to 10000 foreach { i =>
        Seq("wc").exec("1")
        println(s"p2 OK $i")
      }
      success
    }
    */

    "call a program 1" in {
      Seq(Proc.escape("echo -n a"))
        .exec().string mustEqual "a"
    }

    "call a program 2" in {
      Seq(Proc.escape("echo"), "-n", "a")
        .exec().string mustEqual "a"
    }

    "call a program 3" in {
      Seq("echo -n a")
        .exec().string mustEqual "a"
    }

    "call a program" in {
      Seq("echo", "-n", "a")
        .exec().string mustEqual "a"
    }

    "call a program with Int argunent" in {
      Seq("echo", "-n", 1)
        .exec().string mustEqual "1"
    }

    "call a program with arguments" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt")
        .exec().string mustEqual "angel\r\néindʒəl\r\n"
    }

    "call a program with unicode encoded arguments" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt",
        "|", "grep", "éindʒəl")
        .exec().string mustEqual "éindʒəl\n"
    }

    "call a program with standard input" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-")
        .exec("stdin-data").string mustEqual "angel\r\néindʒəl\r\nstdin-data"
    }

    "call a program with unicode encoded standard input" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt", "-")
        .exec("éindʒəl").string mustEqual "angel\r\néindʒəl\r\néindʒəl"
    }

    "connect programs" in {
      Seq("cat", "./src/test/scala/wok/process/resources/angel.txt",
        "|", "grep", "an")
        .exec().string mustEqual "angel\n"
    }
  }
  */
  }
}