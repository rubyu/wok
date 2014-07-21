
package wok.reflect

import tools.nsc.interpreter.AbstractFileClassLoader


case class DynamicClass(classLoader: AbstractFileClassLoader) {
  def create(arg: List[String]) = {
    classLoader.findClass("wok.Wok")
      .getConstructor(classOf[List[String]])
      .newInstance(Nil)
      .asInstanceOf[AbstractWok]
  }
}
