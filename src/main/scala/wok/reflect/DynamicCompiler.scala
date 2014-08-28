
package wok.reflect

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.BatchSourceFile


object DynamicCompiler {

  private def compile(settings: Settings, reporter: StoreReporter, source: String) {
    val global = new Global(settings, reporter)
    new global.Run().compileSources(List(new BatchSourceFile("Wok.scala", source)))
  }

  def compile(before: List[String], process: List[String], after: List[String]): (Report, DynamicClass) = {
    val virtualDirectory = new VirtualDirectory("[memory]", None)
    val settings = new Settings
    settings.deprecation.value = false
    settings.unchecked.value = false
    settings.feature.value = false
    settings.outputDirs.setSingleOutput(virtualDirectory)
    settings.bootclasspath.value = System.getProperty("java.class.path")
    settings.classpath.value = settings.bootclasspath.value
    val source = sourceString(before, process, after)
    val reporter = new StoreReporter
    compile(settings, reporter, source)

    (new Report(source, reporter),
      DynamicClass(new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)))
  }

  def sourceString(before: List[String], process: List[String], after: List[String]): String = {

    def indent(xs: List[String]): Option[String] =
      if (xs.isEmpty) None
      else Some(xs mkString(" " * 4, "\n" + " " * 4, "\n"))

    def script(xs: List[String]): Option[String] =
      if (xs.isEmpty) None
      else Some(
        """|    ;{
          |      var currentRow: Option[Row] = None
          |      def NF = currentRow.get.size
          |      def NR = currentRow.get.id
          |      def FT = currentRow.get.sep
          |      def RT = currentRow.get.term
          |      Stdin #> { _.csv
          |        .map { row => currentRow = Some(row); row } %s
          |      }
          |    };
          |""".stripMargin.format(xs mkString "\n" + " " * 8))

    val b = new StringBuilder
    b.append(
      """|package wok
        |
        |import wok.reflect.AbstractWok
        |import wok.reflect.Helpers._
        |import wok.core.Stdio.{in => Stdin, out => Stdout, err => Stderr}
        |import wok.csv.{Quote, Reader, Row, Writer}
        |import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}
        |import scalax.io.{Codec, Resource}
        |import scalax.file.Path
        |import scalax.file.ImplicitConversions.string2path
        |
        |class Wok(val args: List[String]) extends AbstractWok {
        |  def runScript(): Unit = {""".stripMargin)
    val scripts = List(indent(before), script(process), indent(after))
    if (scripts.exists(_.isDefined)) b.append("\n")
    scripts foreach { s => if (s.isDefined) b.append(s.get) }
    if (scripts.exists(_.isDefined)) b.append(" " * 2)
    b.append(
      """|}
        |}""".stripMargin)
    b.result()
  }
}
