
package scala.sys.patched.process

import java.io._

import org.specs2.mutable._


class ProcTest extends SpecificationWithJUnit {

  /*
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
   */

  /*
  "PipeSink.release() stop " in {

    class TestPipeSource(label: => String) extends scala.sys.process.Process.PipeSource(() => label) {
    }
  }
  */

  "by reflection" in {

    //val theType = getType(classOf[Process.PipeSink])
    //println(s"typeTag.declarations => ${theType.declarations}")
    //println(s"typeTag.members => ${theType.members}")

    //val sink = newInstance(classOf[Process.PipeSink])
    //println(sink)

    //val m  = universe.runtimeMirror(scala.sys.process.Process.getClass.getClassLoader)
    //val oC = universe.typeOf[scala.sys.process.Process.type].termSymbol.asModule
    //val p = m.reflectModule(oC)
    //p.instance.asInstanceOf[Process]
    //val p2 = m.reflect(p)
    //val sink = p2.reflectClass(universe.typeOf[Process.PipeSink].typeSymbol.asClass)
    //val sink_cons_m = universe.typeOf[Process.PipeSink].declaration(universe.nme.CONSTRUCTOR).asMethod
    //val sink_cons = sink.reflectConstructor(sink_cons_m)
    //val s = sink_cons("dummy")
    //println(s)

    /*
    val ru = scala.reflect.runtime.universe
    val rm = ru.runtimeMirror(Thread.currentThread.getContextClassLoader)

    val objectP = ru.typeOf[Process.type].termSymbol.asModule
    val mm = rm.reflectModule(objectP)
    val obj = mm.instance
    println(obj)

    val objectSink = rm.reflect(obj).reflectClass(ru.typeOf[Process.PipeSink].typeSymbol.asClass)
    val ctorSink = ru.typeOf[Process.PipeSink].declaration(ru.nme.CONSTRUCTOR).asMethod
    val ctorsink = objectSink.reflectConstructor(ctorSink)
    println(ctorsink)
    val sink = ctorsink(obj, "aa", "aa")
    //val sink = ctorsink(obj, "dummy")
    println(sink)
    */
    import scala.concurrent.{Future, Await}
    import scala.concurrent.duration.Duration
    import scala.concurrent.duration.SECONDS

    trait CloseCheckingCloseable extends Closeable {
      var closed = false
      override def close() = closed = true
    }
    class DebugOutputStream extends ByteArrayOutputStream with CloseCheckingCloseable
    class DebugInputStream(bytes: Array[Byte]) extends ByteArrayInputStream(bytes) with CloseCheckingCloseable
    class DebugInfinityInputStream extends InputStream with CloseCheckingCloseable {
      def read() = 1
    }


    def sinkPipe(pipeSink: Process.PipeSink) = {
      val field = pipeSink.getClass.getDeclaredField("pipe")
      field.setAccessible(true)
      field.get(pipeSink).asInstanceOf[PipedInputStream]
    }

    def sourcePipe(pipeSource: Process.PipeSource) = {
      val field = pipeSource.getClass.getDeclaredField("pipe")
      field.setAccessible(true)
      field.get(pipeSource).asInstanceOf[PipedOutputStream]
    }

    def throwsIOException(f: => Unit) = {
      try {
        f
        false
      }
      catch {
        case _: IOException => true
      }
    }

    "PipeSource" in {
      "run() should release resources" in {
        val source = new Process.PipeSource("TestPipeSource")
        val in = new DebugInputStream(Array[Byte](1))
        val pout = sourcePipe(source)
        val pin = new PipedInputStream
        pout connect pin
        source.start()
        source connect in
        val f = Future {
          source.join()
        }
        Await.result(f, Duration(5, SECONDS))
        in.closed must beTrue
        throwsIOException { pout.write(1) } must beTrue
      }

      "release() should release resources" in {
        "when waiting for sourse.get()" in {
          val source = new Process.PipeSource("TestPipeSource")
          val pout = sourcePipe(source)
          val pin = new PipedInputStream
          pout connect pin
          source.start()
          val f = Future {
            source.release()
          }
          Await.result(f, Duration(5, SECONDS))
          throwsIOException { pout.write(1) } must beTrue
        }

        "when copying stream" in {
          val source = new Process.PipeSource("TestPipeSource")
          val in = new DebugInfinityInputStream
          val pout = sourcePipe(source)
          val pin = new PipedInputStream
          pout connect pin
          source.start()
          source connect in
          val f = Future {
            while (pin.available() == 0) {
              Thread.sleep(1)
            }
            source.release()
          }
          Await.result(f, Duration(5, SECONDS))
          in.closed must beTrue
          throwsIOException { pout.write(1) } must beTrue
        }
      }
    }

    "PipeSink" in {
      "run() should release resources" in {
        val sink = new Process.PipeSink("TestPipeSink")
        val pout = new PipedOutputStream
        val pin = sinkPipe(sink)
        val out = new DebugOutputStream
        sink connect pout
        sink.start()
        sink connect out
        pout write 1
        pout.close()
        val f = Future {
          sink.join()
        }
        Await.result(f, Duration(5, SECONDS))
        throwsIOException { pin.read() } must beTrue
        out.closed must beTrue
      }

      "release() should release resources" in {
        "when waiting for sink.get()" in {
          val sink = new Process.PipeSink("TestPipeSink")
          val pout = new PipedOutputStream
          val pin = sinkPipe(sink)
          sink connect pout
          sink.start()
          val f = Future {
            sink.release()
          }
          Await.result(f, Duration(5, SECONDS))
          throwsIOException { pin.read() } must beTrue
        }

        "when copying stream" in {
          val sink = new Process.PipeSink("TestPipeSink")
          val pout = new PipedOutputStream
          val pin = sinkPipe(sink)
          val out = new DebugOutputStream
          sink connect pout
          sink.start()
          sink connect out
          pout write 1
          val f = Future {
            while (out.size == 0) {
              Thread.sleep(1)
            }
            sink.release()
          }
          Await.result(f, Duration(5, SECONDS))
          throwsIOException { pin.read() } must beTrue
          out.closed must beTrue
        }
      }
    }
  }

  /*
  "PipedProcesses.runAndExitValue() x10 return value within a second" in {
    import scala.concurrent.{Future, Await}
    import scala.concurrent.duration.Duration
    import scala.concurrent.duration.SECONDS
    import java.io.ByteArrayInputStream

    def input = new ByteArrayInputStream("a".getBytes())

    val f = Future {
      for (i <- 0 until 100) {
        println(s"process $i returns ${ ("wc" #< input).run(ProcessLogger { _ => () }).exitValue() }")
      }
      "OK"
    }
    Await.result(f, Duration(5, SECONDS)) mustEqual "OK"
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
