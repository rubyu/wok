
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

  def compile(script: String): (Report, DynamicClass) = {
    val virtualDirectory = new VirtualDirectory("[memory]", None)
    val settings = new Settings
    settings.deprecation.value = false
    settings.unchecked.value = false
    settings.feature.value = false
    settings.outputDirs.setSingleOutput(virtualDirectory)
    settings.bootclasspath.value = System.getProperty("java.class.path")
    settings.classpath.value = settings.bootclasspath.value
    val source = sourceString(script)
    val reporter = new StoreReporter
    compile(settings, reporter, source)

    (new Report(source, reporter),
      DynamicClass(new AbstractFileClassLoader(virtualDirectory, getClass.getClassLoader)))
  }

  def sourceString(script: String): String = {
    """|package wok
      |
      |import wok.reflect.AbstractWok
      |import wok.reflect.Helpers._
      |import wok.core.Stdio.{in => Stdin, out => Stdout, err => Stderr}
      |import wok.csv.Quote
      |import scala.sys.patched.process.{stringToProcess, stringSeqToProcess}
      |import scalax.io.{Codec, Resource}
      |import scalax.file.Path
      |import scalax.file.ImplicitConversions.string2path
      |
      |class Wok(override val args: List[String]) extends AbstractWok {
      |def runScript(): Unit = {
      |%s
      |}}
      |""".stripMargin.format(script)
  }
}
