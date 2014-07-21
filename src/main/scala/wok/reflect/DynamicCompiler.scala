
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

  def compile(before: List[String], process: String, after: List[String]): DynamicClass = {
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

  private def source(before: List[String], process: String, after: List[String]): String = {
    """
      |package wok
      |
      |import wok.reflect.AbstractWok
      |import wok.reflect.Helpers._
      |
      |class Wok(val arg: List[String]) extends AbstractWok {
      |%s
      |  {
      |    val result = Console.in.open() %s
      |    result.complete()
      |  }
      |%s
      |}
      |"""
      .stripMargin.format(before.map { "  " + _ }.mkString("\n"), process, after.map { "  " + _ }.mkString("\n"))
  }
}
