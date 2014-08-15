
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

  def compile(before: List[String], process: Option[String], after: List[String]): (Report, DynamicClass) = {
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

  private def sourceString(before: List[String], process: Option[String], after: List[String]): String = {

    def indent(xs: List[String]): Option[String] =
      if (xs.isEmpty) None
      else Some(xs map { "    " + _ } mkString("\n" * 2))

    def script(str: Option[String]): Option[String] =
      if (str.isEmpty) None
      else Some(
        """|    {
          |      var currentRow = Row(0, Nil, Nil, "", "")
          |      def NF = currentRow.size
          |      def NR = currentRow.id
          |      def FT = currentRow.sep
          |      def RT = currentRow.term
          |      STDIN #> {
          |        _.csv.map { row => currentRow = row; row } %s
          |      }
          |    }""".stripMargin.format(str.get))

    val b = new StringBuilder
    b.append(
      """|package wok
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
        |class Wok(val args: List[String]) extends AbstractWok {
        |  def runScript(): Unit = {""".stripMargin)
    val scripts = List(indent(before), script(process), indent(after))
    if (scripts.exists(_.isDefined))
      b.append("\n")
    scripts.zipWithIndex.map { case (s, idx) =>
      if (s.isDefined) {
        b.append(s.get)
        b.append("\n")
        if (scripts.drop(idx+1).exists(_.isDefined))
          b.append("\n")
      }
    }
    if (scripts.exists(_.isDefined))
      b.append("  ")
    b.append(
      """|}
        |}""".stripMargin)
    b.result()
  }
}
