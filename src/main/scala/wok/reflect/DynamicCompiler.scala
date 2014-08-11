
package wok.reflect

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.util.BatchSourceFile


object DynamicCompiler {

  private def compile(settings: Settings, source: String) {
    val global = new Global(settings, new ConsoleReporter(settings))
    new global.Run().compileSources(List(new BatchSourceFile("Wok.scala", source)))
  }

  def compile(before: List[String], process: Option[String], after: List[String]): DynamicClass = {
    val virtualDirectory = new VirtualDirectory("[memory]", None)
    val settings = new Settings
    settings.deprecation.value = true
    settings.unchecked.value = true
    settings.outputDirs.setSingleOutput(virtualDirectory)
    settings.bootclasspath.value = System.getProperty("java.class.path")
    settings.classpath.value = settings.bootclasspath.value

    compile(settings, source(before, process, after))
    DynamicClass(new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader))
  }

  private def source(before: List[String], process: Option[String], after: List[String]): String = {

    def indent(xs: List[String]) = xs.map { "    " + _ } .mkString("\n")

    def script(str: Option[String]) = {
      if (str.isDefined) {
        """|      var currentRow = Row(0, Nil, Nil, "", "")
          |      def NF = currentRow.size
          |      def NR = currentRow.id
          |      def FT = currentRow.sep
          |      def RT = currentRow.term
          |      STDIN #> {
          |        _.csv.map { row => currentRow = row; row } %s
          |      }
        """.stripMargin.format(str.get)
      } else {
        "// no script"
      }
    }

    """
      |package wok
      |
      |import wok.reflect.AbstractWok
      |import wok.reflect.Helpers._
      |import wok.core.Stdio.{in => STDIN, out => STDOUT, err => STDERR}
      |import wok.csv.{Quote, Reader, Row, Writer}
      |import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}
      |import scalax.io.{Codec, Resource}
      |import scalax.file.Path
      |import scalax.file.ImplicitConversions.string2path
      |
      |
      |class Wok(val args: List[String]) extends AbstractWok {
      |  def runScript() {
      |
      |%s
      |
      |    {
      |
      |%s
      |
      |    }
      |
      |%s
      |
      |  }
      |}
      |""".stripMargin.format(indent(before), script(process), indent(after))
  }
}
