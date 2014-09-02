
package wok.core

import java.util.concurrent.ForkJoinPool

class ThreadSafeExecutor extends ForkJoinPool {
  private val threadClass = classOf[Thread]
  private val threadLocalClass = classOf[ThreadLocal[_]]
  private val threadLocalMapClass = ClassLoader.getSystemClassLoader.loadClass("java.lang.ThreadLocal$ThreadLocalMap")

  private val threadLocals = threadClass.getDeclaredField("threadLocals")
  threadLocals.setAccessible(true)

  private val inheritableThreadLocals = threadClass.getDeclaredField("inheritableThreadLocals")
  inheritableThreadLocals.setAccessible(true)

  private val createInheritedMap = threadLocalClass.getDeclaredMethod("createInheritedMap", threadLocalMapClass)
  createInheritedMap.setAccessible(true)

  override def execute(task: Runnable) {
    val parent = Thread.currentThread()
    super.execute(new Runnable {
      override def run() {
        val current = Thread.currentThread()
        // initialize threadLocals
        threadLocals.set(current, null)
        // initialize inheritableThreadLocals
        val parentMap = inheritableThreadLocals.get(parent)
        val currentMap = createInheritedMap.invoke(current, parentMap)
        inheritableThreadLocals.set(current, currentMap)

        task.run()
      }
    })
  }
}