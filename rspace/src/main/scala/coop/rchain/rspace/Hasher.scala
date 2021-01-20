package coop.rchain.rspace

import java.nio.charset.StandardCharsets

import coop.rchain.rspace.internal.{codecByteVector, codecSeq, RichAttempt}
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.bits.BitVector

object Hasher {
  val joinSuffixBits         = BitVector("-joins".getBytes(StandardCharsets.UTF_8))
  val dataSuffixBits         = BitVector("-data".getBytes(StandardCharsets.UTF_8))
  val continuationSuffixBits = BitVector("-continuation".getBytes(StandardCharsets.UTF_8))

  def hashWithSuffix(bits: BitVector, suffix: BitVector): Blake2b256Hash = {
    val suffixed = bits ++ suffix
    Blake2b256Hash.create(suffixed.toByteVector)
  }

  def hashJoinsChannel[C](channel: C, codeC: Codec[C]): Blake2b256Hash =
    hashWithSuffix(codeC.encode(channel).get, joinSuffixBits)

  def hashContinuationsChannels[C](channels: Seq[C], serializeC: Serialize[C]): Blake2b256Hash = {
    val chs = channels.par
      .map(c => serializeC.encode(c))
      .toList
      .sorted(util.ordByteVector)
    val channelsBits = codecSeq(codecByteVector).encode(chs).get
    hashWithSuffix(channelsBits, continuationSuffixBits)
  }

  def hashDataChannel[C](channel: C, codeC: Codec[C]): Blake2b256Hash =
    hashWithSuffix(codeC.encode(channel).get, dataSuffixBits)
}
