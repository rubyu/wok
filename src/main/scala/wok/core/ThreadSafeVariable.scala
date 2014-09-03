/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package wok.core

import java.lang.InheritableThreadLocal

class ThreadSafeVariable[T](init: T, copy: T => T = {t: T => t}) {
  private val tl = new InheritableThreadLocal[T] {
    override def initialValue = init.asInstanceOf[T with AnyRef]
    override def childValue(parentValue: T): T = copy(parentValue).asInstanceOf[T with AnyRef]
  }

  def withCopy(f: T => T) = new ThreadSafeVariable[T](init, f)

  /** Retrieve the current value */
  def value: T = tl.get.asInstanceOf[T]

  /** Set the value of the variable while executing the specified
    * thunk.
    *
    * @param newval The value to which to set the variable
    * @param thunk The code to evaluate under the new setting
    */
  def withValue[S](newval: T)(thunk: => S): S = {
    val oldval = value
    tl set newval

    try thunk
    finally tl set oldval
  }

  /** Change the currently bound value, discarding the old value.
    * Usually withValue() gives better semantics.
    */
  def value_=(newval: T) = tl set newval

  override def toString: String = "ThreadSafeVariable(" + value + ")"
}
