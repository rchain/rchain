package coop.rchain.rspace.hashing

import coop.rchain.rspace.serializers.ScodecSerialize.{
  codecSeqByteVector,
  toOrderedByteVectors,
  RichAttempt
}
import coop.rchain.rspace.util
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}

import java.nio.charset.StandardCharsets

object ChannelHash {
//  private val joinSuffixBits         = BitVector("-joins".getBytes(StandardCharsets.UTF_8))
//  private val dataSuffixBits         = BitVector("-data".getBytes(StandardCharsets.UTF_8))
//  private val continuationSuffixBits = BitVector("-continuation".getBytes(StandardCharsets.UTF_8))
//
//  private def hashWithSuffix(bits: BitVector, suffix: BitVector): Blake2b256Hash = {
//    val suffixed = bits ++ suffix
//    Blake2b256Hash.create(suffixed.toByteVector)
//  }

  def hashJoinsChannel[C](channel: C, serializeC: Serialize[C]): Blake2b256Hash = {
//    val cc = serializeC.toSizeHeadCodec
//    hashWithSuffix(cc.encode(channel).getUnsafe, joinSuffixBits)

    // TODO: preparation for hard fork refactoring (direct use of Serialize[C])
//     hashWithSuffix(serializeC.encode(channel).toBitVector, joinSuffixBits)
    val channelEncoded = serializeC.encode(channel)
    Blake2b256Hash.create(channelEncoded)
  }

  def hashContinuationsChannels[C](channels: Seq[C], serializeC: Serialize[C]): Blake2b256Hash = {
//    // Serialize[C] => Codec[C]
//    val codecC = serializeC.toSizeHeadCodec
//    // Codec[C] => Serialize[C]
//    val serializeC2 = fromCodec(codecC)
//    val chs         = toOrderedByteVectors(channels)(serializeC2)

    // TODO: preparation for hard fork refactoring (direct use of Serialize[C])
    // val chs = toOrderedByteVectors(channels)(serializeC)

//    val channelsBits = codecSeqByteVector.encode(chs).getUnsafe
//    hashWithSuffix(channelsBits, continuationSuffixBits)

//    val chs             = toOrderedByteVectors(channels)(serializeC)
//    val channelsEncoded = codecSeqByteVector.encode(chs).getUnsafe
//    Blake2b256Hash.create(channelsEncoded.toByteVector)

    val channelsHashes = toOrderedByteVectors(channels)(serializeC).map(Blake2b256Hash.create)
    hashContinuationsChannels(channelsHashes)
  }

  def hashContinuationsChannels(channelsHashes: Seq[Blake2b256Hash]): Blake2b256Hash = {
    val ordHashes = channelsHashes.map(_.bytes).sorted(util.ordByteVector)
    Blake2b256Hash.create(ordHashes)
  }

  def hashDataChannel[C](channel: C, serializeC: Serialize[C]): Blake2b256Hash = {
//    val cc = serializeC.toSizeHeadCodec
//    hashWithSuffix(cc.encode(channel).getUnsafe, dataSuffixBits)

    // TODO: preparation for hard fork refactoring (direct use of Serialize[C])
    // hashWithSuffix(serializeC.encode(channel).toBitVector, dataSuffixBits)

    val channelEncoded = serializeC.encode(channel)
    Blake2b256Hash.create(channelEncoded)
  }
}
