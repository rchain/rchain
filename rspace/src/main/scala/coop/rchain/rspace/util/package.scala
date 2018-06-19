package coop.rchain.rspace

import coop.rchain.rspace.internal.{Datum, GNAT, WaitingContinuation}

import scala.collection.immutable.Seq

package object util {

  /**
    * Extracts a continuation from a produce result
    */
  def getK[A, K](t: Option[(K, A)]): K =
    t.map(_._1).get

  /** Runs a continuation with the accompanying data
    */
  def runK[T](t: Option[((T) => Unit, T)]): Unit =
    t.foreach { case (k, data) => k(data) }

  /** Runs a list of continuations with the accompanying data
    */
  def runKs[T](t: Seq[Option[((T) => Unit, T)]]): Unit =
    t.foreach { case Some((k, data)) => k(data); case None => () }

  /**
    * Compare to `memcmp` in C/C++
    *
    * Based on:
    * [[https://github.com/OpenTSDB/asynchbase/blob/f6a8ccb7e55ed9bc0aad265345da4c679e750055/src/Bytes.java#L549-L572]]
    */
  @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Return"))
  def memcmp(a: Array[Byte], b: Array[Byte]): Int = {
    val length = Math.min(a.length, b.length)
    if (java.util.Arrays.equals(a, b)) {
      0
    } else {
      for (i <- 0 to length) {
        val ai = a(i)
        val bi = b(i)
        if (ai != bi) {
          return (ai & 0xFF) - (bi & 0xFF)
        }
      }
      a.length - b.length
    }
  }

  def canonicalize[C, P, A, K](gnat: GNAT[C, P, A, K]): GNAT[C, P, A, K] = {

    implicit val ordArrayByte: Ordering[Array[Byte]] = (x: Array[Byte], y: Array[Byte]) =>
      memcmp(x, y)

    gnat.copy(
      wks = gnat.wks.sortBy(_.source.hash.bytes.toArray),
      data = gnat.data.sortBy(_.source.hash.bytes.toArray)
    )
  }
}
