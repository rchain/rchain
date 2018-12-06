package coop.rchain.rspace

import internal._
import scala.collection.immutable.Seq
import cats.implicits._
import scodec.bits.ByteVector
import scodec.{Attempt, Codec, Encoder}
import scodec.bits.BitVector
import scodec.codecs._
import scodec.interop.cats._

object StableHashProvider {

  def hash[C](channel: C)(implicit serializeC: Serialize[C]): Blake2b256Hash =
    Blake2b256Hash.create(serializeC.encode(channel))

  def hash[C](channels: Seq[C])(implicit serializeC: Serialize[C]): Blake2b256Hash =
    Blake2b256Hash.create(
      channels
        .map(c => serializeC.encode(c))
        .sorted(util.ordByteVector)
    )

  def hashMany[C](channels: Seq[C])(implicit serializeC: Serialize[C]): Seq[Blake2b256Hash] =
    toOrderedByteVectors(channels).map(Blake2b256Hash.create)

  def hash[C, P, K](
      encodedChannels: Seq[ByteVector],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean
  )(
      implicit
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ) = {

    val encodedPatterns =
      patterns
        .map { serializeP.encode(_) }
        .sorted(util.ordByteVector)

    Blake2b256Hash.create(
      encodedChannels ++ encodedPatterns
        ++ List(
          serializeK.encode(continuation),
          (ignore(7) ~> bool).encode(persist).map(_.bytes).get
        )
    )
  }

  def hash[C, A](channel: C, datum: A, persist: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeA: Serialize[A]
  ) =
    Blake2b256Hash.create(
      Seq(
        serializeC.encode(channel),
        serializeA.encode(datum),
        (ignore(7) ~> bool).encode(persist).map(_.bytes).get
      )
    )
}
