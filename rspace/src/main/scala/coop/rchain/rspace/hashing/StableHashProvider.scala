package coop.rchain.rspace.hashing

import coop.rchain.rspace.serializers.ScodecSerialize.{
  codecSeqByteVector,
  toOrderedByteVectors,
  RichAttempt
}
import coop.rchain.rspace.{util, Channel}
import coop.rchain.shared.Serialize
import scodec.codecs._

object StableHashProvider {

  def hashChannelHashes(channelsHashes: Seq[Blake2b256Hash]): Blake2b256Hash = {
    val ordHashes = channelsHashes.map(_.bytes).sorted(util.ordByteVector)
    Blake2b256Hash.create(ordHashes)
  }

  def hash[C](channel: C)(implicit serializeC: Serialize[C]): Blake2b256Hash =
    Blake2b256Hash.create(serializeC.encode(channel))

  def hash[C](channels: Seq[C])(implicit serializeC: Serialize[C]): Blake2b256Hash = {
//    val encodedChannels = toOrderedByteVectors(channels).toVector
//    Blake2b256Hash.create(encodedChannels)
    val channelsEncoded = channels.map(hash(_))
    hashChannelHashes(channelsEncoded)
  }

  def hash[P, K](
      channels: Seq[Channel],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean
  )(
      implicit
      serializeP: Serialize[P],
      serializeK: Serialize[K]
  ) = {
    val encodedChannels = channels.map(_.hash.bytes)
    val encodedPatterns = toOrderedByteVectors(patterns)(serializeP)
    val encodedCont     = serializeK.encode(continuation)
    val encodedPersist  = (ignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
    val encodedSeq      = encodedChannels ++ encodedPatterns :+ encodedCont :+ encodedPersist
    val encoded         = codecSeqByteVector.encode(encodedSeq).getUnsafe.toByteVector
//    Blake2b256Hash.create(encodedSeq)
    Blake2b256Hash.create(encoded)
  }

  def hash[A](channel: Channel, datum: A, persist: Boolean)(
      implicit
      serializeA: Serialize[A]
  ) = {
    val encodedSeq = Seq(
      channel.hash.bytes,
      serializeA.encode(datum),
      (ignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
    )
    val encoded = codecSeqByteVector.encode(encodedSeq).getUnsafe.toByteVector
//    Blake2b256Hash.create(encodedSeq)
    Blake2b256Hash.create(encoded)
  }
}
