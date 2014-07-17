
package wok.process

import java.io.InputStream
import scala.sys.process._
import annotation.tailrec
import concurrent.SyncVar


class Process(commandStrings: Seq[String]) {
  private def parse(commandStrings: Seq[String]) = {
    @tailrec
    def split(list: Seq[Seq[String]], strings: Seq[String]): Seq[Seq[String]] = {
      strings.indexOf("|") match {
        case 0 => throw new IllegalArgumentException("command must not be empty")
        case -1 if strings.isEmpty => throw new IllegalArgumentException("command must not be empty")
        case -1 => list :+ strings
        case n => split(list :+ strings.take(n), strings.drop(n+1))
      }
    }
    split(Seq[Seq[String]](), commandStrings)
  }

  def exec() = executeCommands()
  def exec(bytes: Array[Byte]) = executeCommands(bytes)

  private def streamToBytes(in: InputStream) = Stream.continually(in.read()).takeWhile(-1 !=).map(_.toByte).toArray

  private def executeCommands(input: Array[Byte] = Array[Byte]()): Array[Byte] = {
    def execute(input: Array[Byte], command: Seq[String]): Array[Byte] = {
      val in = new SyncVar[Unit]
      val out = new SyncVar[Array[Byte]]
      val err = new SyncVar[Array[Byte]]

      val p = command.run(new ProcessIO(
        stdin => { try { in put stdin.write(input) } finally { stdin.close() } },
        stdout => { try { out put streamToBytes(stdout) } finally { stdout.close() } },
        stderr => { try { err put streamToBytes(stderr) } finally { stderr.close() } }
      ))
      try {
        in.get
        p.exitValue() match {
          case code if code != 0 => throw new RuntimeException(s"process returns non zero exit code: ${code}")
          case _ =>
        }
        err.get match {
          case bytes if bytes.size > 0 => Console.err.write(bytes)
          case _ =>
        }
        out.get
      } finally {
        p.destroy()
      }
    }

    @tailrec
    def executeSeq(input: Array[Byte], commands: Seq[Seq[String]]): Array[Byte] = {
      commands match {
        case head :: Nil => execute(input, head)
        case head :: tail => executeSeq(execute(input, head), tail)
      }
    }
    executeSeq(input, parse(commandStrings))
  }
}