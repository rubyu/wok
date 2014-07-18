
package wok.process

import java.io.ByteArrayInputStream
import scala.sys.process._
import annotation.tailrec
import concurrent.SyncVar
import scalax.io.{Codec, Resource}
import wok.core.Helpers.{EscapedString, QuotedString}


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
  def exec(s: String)(implicit cd: Codec = Codec.default) = executeCommands(cd.encode(s))

  private def executeCommands(input: Array[Byte] = Array[Byte]()): Result = {
    def execute(input: Array[Byte], command: Seq[String]): Result = {
      val out, err = new SyncVar[Array[Byte]]

      val p = command.run(new ProcessIO(
        stdin => { try stdin.write(input) finally stdin.close() },
        stdout => { try out put Resource.fromInputStream(stdout).byteArray finally stdout.close() },
        stderr => { try err put Resource.fromInputStream(stderr).byteArray finally stderr.close() }))

      try {
        Result(p.exitValue(),
          Resource.fromInputStream(new ByteArrayInputStream(out.get)),
          Resource.fromInputStream(new ByteArrayInputStream(err.get)))
      } finally {
        p.destroy()
      }
    }

    @tailrec
    def executeSeq(input: Array[Byte], commands: Seq[Seq[String]]): Result = {
      commands match {
        case head :: Nil => execute(input, head)
        case head :: tail => executeSeq(execute(input, head).out.byteArray, tail)
      }
    }
    executeSeq(input, parse(commandStrings))
  }
}


object Process {
  def escape(s: String) = s.escaped('\\', '"').quoted('"')
}
