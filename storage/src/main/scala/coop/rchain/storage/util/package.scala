package coop.rchain.storage

import java.nio.ByteBuffer

package object util {

  /**
    * Runs a computation for its side-effects, discarding its value
    *
    * @param a A computation to run
    */
  def ignore[A](a: => A): Unit = {
    val _: A = a
    ()
  }

  /**
    * Executes a function `f` with a given [[AutoCloseable]] `a` as its argument,
    * returning the result of the function and closing the `a`
    *
    * Compare to Java's "try-with-resources"
    *
    * @param a A given resource implementing [[AutoCloseable]]
    * @param f A function that takes this resource as its argument
    */
  def withResource[A <: AutoCloseable, B](a: A)(f: A => B): B =
    try {
      f(a)
    } finally {
      a.close()
    }

  /** Drops the 'i'th element of a list.
    */
  def dropIndex[T](xs: List[T], n: Int): List[T] = {
    val (l1, l2) = xs splitAt n
    l1 ++ (l2 drop 1)
  }

  /**
    * Converts specified byteBuffer to '-' separated string,
    * convenient during debugging
    */
  private[storage] def toStr(byteBuffer: ByteBuffer): String = {
    byteBuffer.mark()
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore { byteBuffer.get(fetched) }
    byteBuffer.reset()
    fetched.toSeq.map(x => x.toString).mkString("-")
  }
}
