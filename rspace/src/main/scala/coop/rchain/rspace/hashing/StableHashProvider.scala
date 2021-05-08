package coop.rchain.rspace.hashing

import coop.rchain.rspace.serializers.ScodecSerialize.RichAttempt
import coop.rchain.rspace.util
import coop.rchain.shared.Serialize
import scodec.bits.ByteVector
import scodec.codecs._
import coop.rchain.crypto.Blake2b256Hash

object StableHashProvider {

  def hash[C](channel: C)(implicit serializeC: Serialize[C]): Blake2b256Hash =
    Blake2b256Hash.create(serializeC.encode(channel))

  def hash[C](channels: Seq[C])(implicit serializeC: Serialize[C]): Blake2b256Hash =
    Blake2b256Hash.create(
      channels
        .map(serializeC.encode)
        .sorted(util.ordByteVector)
    )

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
        .map(serializeP.encode)
        .sorted(util.ordByteVector)

    Blake2b256Hash.create(
      encodedChannels ++ encodedPatterns
        ++ List(
          serializeK.encode(continuation),
          (ignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
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
        (ignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
      )
    )
}
