package coop.rchain.rspace

import coop.rchain.shared.Serialize
import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets
import scala.util.Try

package object util {

  implicit def unpackSeq[C, P, K, R](
      v: Seq[Option[(ContResult[P, K], Seq[Result[R]])]]
  ): Seq[Option[(K, Seq[R])]] =
    v.map(unpackOption)

  implicit def unpackOption[C, P, K, R](
      v: Option[(ContResult[P, K], Seq[Result[R]])]
  ): Option[(K, Seq[R])] =
    v.map(unpackTuple)

  implicit def unpackTuple[C, P, K, R](
      v: (ContResult[P, K], Seq[Result[R]])
  ): (K, Seq[R]) =
    v match {
      case (ContResult(continuation, _, _, _, _), data) =>
        (continuation, data.map(_.matchedDatum))
    }

  implicit def unpackOptionWithPeek[P, K, R](
      v: Option[(ContResult[P, K], Seq[Result[R]])]
  ): Option[(K, Seq[(Channel, R, R, Boolean)], Boolean)] =
    v.map(unpackTupleWithPeek)

  implicit def unpackTupleWithPeek[P, K, R](
      v: (ContResult[P, K], Seq[Result[R]])
  ): (K, Seq[(Channel, R, R, Boolean)], Boolean) =
    v match {
      case (ContResult(continuation, _, _, _, peek), data) =>
        (
          continuation,
          data.map(d => (d.channel, d.matchedDatum, d.removedDatum, d.persistent)),
          peek
        )
    }

  implicit def unpackCont[C, P, T](v: ContResult[P, T]): T = v.continuation

  /**
    * Extracts a continuation from a produce result
    */
  def getK[A, K](t: Option[(K, A)]): K =
    t.map(_._1).get

  /** Runs a continuation with the accompanying data
    */
  def runK[T](e: Option[((T) => Unit, T)]): Unit =
    e.foreach { case (k, data) => k(data) }

  /** Runs a list of continuations with the accompanying data
    */
  def runKs[T](t: Seq[Option[((T) => Unit, T)]]): Unit =
    t.foreach { case Some((k, data)) => k(data); case None => () }

  @SuppressWarnings(Array("org.wartremover.warts.Return"))
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

  val ordByteVector: Ordering[ByteVector] = (a: ByteVector, b: ByteVector) => veccmp(a, b)

  val stringSerialize: Serialize[String] = new Serialize[String] {

    def encode(a: String): ByteVector =
      ByteVector.view(a.getBytes(StandardCharsets.UTF_8))

    def decode(bytes: ByteVector): Either[Throwable, String] =
      Try.apply(new String(bytes.toArray, StandardCharsets.UTF_8)).toEither
  }
}
