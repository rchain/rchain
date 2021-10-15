package coop.rchain.rspace.hashing

import coop.rchain.rspace.serializers.ScodecSerialize.{
  codecSeqByteVector,
  toOrderedByteVectors,
  RichAttempt
}
import coop.rchain.rspace.util
import coop.rchain.shared.Serialize
import scodec.bits.ByteVector
import scodec.codecs._

object StableHashProvider {

  def hash[C](channel: C)(implicit serializeC: Serialize[C]): Blake2b256Hash =
    Blake2b256Hash.create(serializeC.encode(channel))

  def hashSeq[C](channels: Seq[C])(implicit serializeC: Serialize[C]): Seq[Blake2b256Hash] =
    channels
      .map(serializeC.encode)
      .map(Blake2b256Hash.create)
      .sortBy(_.bytes)(util.ordByteVector)

  def hash[C](channels: Seq[C])(implicit serializeC: Serialize[C]): Blake2b256Hash =
//    Blake2b256Hash.create(
//      channels
//        .map(serializeC.encode)
//        .sorted(util.ordByteVector)
//    )
    hash(hashSeq(channels))

  def hash(channelsHashes: Seq[Blake2b256Hash]): Blake2b256Hash = {
    val ordHashes = channelsHashes.map(_.bytes).sorted(util.ordByteVector)
    Blake2b256Hash.create(ordHashes)
  }

  def hash[P, K](
      encodedChannels: Seq[ByteVector],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean
  )(implicit serializeP: Serialize[P], serializeK: Serialize[K]) = {
//    val encodedPatterns =
//      patterns
//        .map(serializeP.encode)
//        .sorted(util.ordByteVector)
//    Blake2b256Hash.create(
//      encodedChannels ++ encodedPatterns
//        ++ List(
//        serializeK.encode(continuation),
//        (ignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
//      )
//    )
    val encodedPatterns = toOrderedByteVectors(patterns)(serializeP)
    val encodedCont     = serializeK.encode(continuation)
    val encodedPersist  = bool(8).encode(persist).map(_.bytes).getUnsafe
    val encodedSeq      = encodedChannels ++ encodedPatterns :+ encodedCont :+ encodedPersist
//    Blake2b256Hash.create(encodedSeq)
    val encoded = codecSeqByteVector.encode(encodedSeq).getUnsafe.toByteVector
    Blake2b256Hash.create(encoded)
  }

  def hash[A](channel: ByteVector, datum: A, persist: Boolean)(
      implicit serializeA: Serialize[A]
  ) = {
    //    Blake2b256Hash.create(
    //      Seq(
    //        serializeC.encode(channel),
    //        serializeA.encode(datum),
    //        (ignore(7) ~> bool).encode(persist).map(_.bytes).getUnsafe
    //      )
    //    )
    val encodedSeq = Seq(
      channel,
      serializeA.encode(datum),
      bool(8).encode(persist).map(_.bytes).getUnsafe
    )
//    Blake2b256Hash.create(encodedSeq)
    val encoded = codecSeqByteVector.encode(encodedSeq).getUnsafe.toByteVector
    Blake2b256Hash.create(encoded)
  }
}
