package coop.rchain.rspace

import coop.rchain.rspace.internal.GNAT
import scodec.bits.ByteVector

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
  def memcmp(a: Array[Byte], b: Array[Byte]): Int = {
    val c = a.length - b.length
    if (c != 0) {
      c
    } else {
      for (i <- 0 until a.length) {
        val ai = a(i)
        val bi = b(i)
        if (ai != bi) {
          return (ai & 0xFF) - (bi & 0xFF)
        }
      }
      0
    }
  }

  def canonicalize[C, P, A, K](gnat: GNAT[C, P, A, K]): GNAT[C, P, A, K] =
    gnat.copy(
      wks = gnat.wks.sortBy(_.source.hash.bytes)(ordByteVector),
      data = gnat.data.sortBy(_.source.hash.bytes)(ordByteVector)
    )

  def veccmp(a: ByteVector, b: ByteVector): Int = {
    val c = a.length - b.length
    if (c != 0) {
      c.toInt
    } else {
      for (i <- 0L until a.length) {
        //indexed access of two ByteVectors can be not fast enough,
        //however it is used by ByteVector creators (see === implementation)
        val ai = a(i)
        val bi = b(i)
        if (ai != bi) {
          return (ai & 0xFF) - (bi & 0xFF)
        }
      }
      0
    }
  }

  val ordArrayByte: Ordering[Array[Byte]] = (x: Array[Byte], y: Array[Byte]) => memcmp(x, y)

  val ordByteVector: Ordering[ByteVector] = (a: ByteVector, b: ByteVector) => veccmp(a, b)

  val ordByteVectorPair: Ordering[(ByteVector, ByteVector)] =
    (a: (ByteVector, ByteVector), b: (ByteVector, ByteVector)) =>
      veccmp(a._1, b._1) match {
        case 0 => veccmp(a._2, b._2)
        case c => c
    }
}
