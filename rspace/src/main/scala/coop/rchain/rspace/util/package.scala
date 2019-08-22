package coop.rchain.rspace

import java.nio.charset.StandardCharsets

import cats.Functor
import coop.rchain.rspace.internal.GNAT
import scodec.Codec
import scodec.bits.ByteVector

import scala.util.Try

package object util {

  implicit def unpackSeq[C, P, K, R](
      v: Seq[Option[(ContResult[C, P, K], Seq[Result[R]])]]
  ): Seq[Option[(K, Seq[R], Int)]] =
    v.map(unpackOption)

  implicit def unpackEither[C, P, E, K, R](
      v: Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]
  ): Either[E, Option[(K, Seq[R], Int)]] =
    v.map(unpackOption)

  implicit def unpackEitherF[F[_], C, P, E, K, R](
      v: F[Either[E, Option[(ContResult[C, P, K], Seq[Result[R]])]]]
  )(implicit ev: Functor[F]): F[Either[E, Option[(K, Seq[R], Int)]]] =
    ev.map(v)(_.map(unpackOption))

  implicit def unpackOption[C, P, K, R](
      v: Option[(ContResult[C, P, K], Seq[Result[R]])]
  ): Option[(K, Seq[R], Int)] =
    v.map(unpackTuple)

  implicit def unpackOptionF[F[_], C, P, K, R](
      v: F[Option[(ContResult[C, P, K], Seq[Result[R]])]]
  )(implicit ev: Functor[F]): F[Option[(K, Seq[R], Int)]] =
    ev.map(v)(unpackOption)

  implicit def unpackTuple[C, P, K, R](v: (ContResult[C, P, K], Seq[Result[R]])): (K, Seq[R], Int) =
    v match {
      case (ContResult(continuation, _, _, _, sequenceNumber, _), data) =>
        (continuation, data.map(_.matchedDatum), sequenceNumber)
    }

  implicit def unpackOptionWithPeek[C, P, K, R](
      v: Option[(ContResult[C, P, K], Seq[Result[R]])]
  ): Option[(K, Seq[R], Int, Boolean)] =
    v.map(unpackTupleWithPeek)

  implicit def unpackTupleWithPeek[C, P, K, R](
      v: (ContResult[C, P, K], Seq[Result[R]])
  ): (K, Seq[R], Int, Boolean) =
    v match {
      case (ContResult(continuation, _, _, _, sequenceNumber, peek), data) =>
        (continuation, data.map(_.matchedDatum), sequenceNumber, peek)
    }

  implicit def unpack[T](v: Result[T]): T                     = v.matchedDatum
  implicit def unpackCont[C, P, T](v: ContResult[C, P, T]): T = v.continuation

  /**
    * Extracts a continuation from a produce result
    */
  def getK[A, K](t: Option[(K, A, Int)]): K =
    t.map(_._1).get

  def getK[A, K](e: Either[_, Option[(K, A, Int)]]): K =
    e.map(_.map(_._1).get).right.get

  /** Runs a continuation with the accompanying data
    */
  def runK[T](e: Option[((T) => Unit, T, Int)]): Unit =
    e.foreach { case (k, data, _) => k(data) }

  /** Runs a list of continuations with the accompanying data
    */
  def runKs[T](t: Seq[Option[((T) => Unit, T, Int)]]): Unit =
    t.foreach { case Some((k, data, _)) => k(data); case None => () }

  def canonicalize[C, P, A, K](gnat: GNAT[C, P, A, K]): GNAT[C, P, A, K] =
    gnat.copy(
      wks = gnat.wks.sortBy(_.source.hash.bytes)(ordByteVector),
      data = gnat.data.sortBy(_.source.hash.bytes)(ordByteVector)
    )

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

  val stringCodec: Codec[String] = stringSerialize.toCodec
}
