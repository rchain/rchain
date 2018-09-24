package coop.rchain.rspace

import internal._
import scala.collection.immutable.Seq
import cats.implicits._
import scodec.{Attempt, Codec}
import scodec.bits.BitVector
import scodec.codecs._
import scodec.interop.cats._

object StableHashProvider {
  def hash[C](channel: C)(implicit codecC: Codec[C]): Blake2b256Hash =
    Blake2b256Hash.create(codecC.encode(channel).get.bytes :: Nil)

  def hash[C](channels: Seq[C])(implicit codecC: Codec[C]): Blake2b256Hash =
    Blake2b256Hash.create(
      channels
        .map(c => codecC.encode(c).get.bytes)
        .sorted(util.ordByteVector)
    )

  def hash[C, P, K](channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ) = {
    val (encodedChannels, encodedPatterns) = channels
      .zip(patterns)
      .map { case (channel, pattern) => (serializeC.encode(channel), serializeP.encode(pattern)) }
      .sorted(util.ordByteVectorPair)
      .unzip

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
