package coop.rchain.rspace

import cats.Functor
import coop.rchain.rspace.internal.GNAT
import scodec.bits.ByteVector

import scala.collection.immutable.Seq

package object util {

  implicit def unpackSeq[C, P, K, R](
      v: Seq[Option[(ContResult[C, P, K], Seq[Result[R]])]]
  ): Seq[Option[(K, Seq[R], Int)]] =
    v.map(unpackOption)

  implicit def unpackEither[C, P, E, K, R](
      v: Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]
  ): Either[E, Option[(K, Seq[R], Int)]] =
    v.map(unpackOption)

  implicit def unpackEither[F[_], C, P, E, K, R](
      v: F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]]
  )(implicit ev: Functor[F]): F[Either[E, Option[(K, Seq[R], Int)]]] =
    ev.map(v)(_.map(unpackOption))

  implicit def unpackOption[C, P, K, R](
      v: Option[(ContResult[C, P, K], Seq[Result[R]])]
  ): Option[(K, Seq[R], Int)] =
    v.map(unpackTuple)

  implicit def unpackTuple[C, P, K, R](v: (ContResult[C, P, K], Seq[Result[R]])): (K, Seq[R], Int) =
    v match {
      case (ContResult(continuation, _, _, _, sequenceNumber), data) =>
        (continuation, data.map(_.value), sequenceNumber)
    }

  implicit def unpack[T](v: Result[T]): T                     = v.value
  implicit def unpackCont[C, P, T](v: ContResult[C, P, T]): T = v.value

  /**
    * Extracts a continuation from a produce result
    */
  def getK[A, K](t: Option[(K, A)]): K =
    t.map(_._1).get

  def getK[A, K](e: Either[_, Option[(K, A)]]): K =
    e.map(_.map(_._1).get).right.get

  /** Runs a continuation with the accompanying data
    */
  def runK[T](e: Either[Nothing, Option[((T) => Unit, T, Int)]]): Unit =
    e.right.get.foreach { case (k, data, _) => k(data) }

  /** Runs a list of continuations with the accompanying data
    */
  def runKs[T](t: Seq[Option[((T) => Unit, T, Int)]]): Unit =
    t.foreach { case Some((k, data, _)) => k(data); case None => () }

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
