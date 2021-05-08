package coop.rchain.rspace.hashing

import coop.rchain.models.Par
import coop.rchain.rspace.serializers.ScodecSerialize.{
  codecSeqByteVector,
  toOrderedByteVectors,
  RichAttempt
}
import coop.rchain.shared.Serialize
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import cats.syntax.all._
import coop.rchain.crypto.Blake2b256Hash

import java.nio.charset.StandardCharsets

object ChannelHash {
  private val joinSuffixBits         = BitVector("-joins".getBytes(StandardCharsets.UTF_8))
  private val dataSuffixBits         = BitVector("-data".getBytes(StandardCharsets.UTF_8))
  private val continuationSuffixBits = BitVector("-continuation".getBytes(StandardCharsets.UTF_8))

  private def hashWithSuffix(bits: BitVector, suffix: BitVector): Blake2b256Hash = {
    val suffixed = bits ++ suffix
    Blake2b256Hash.create(suffixed.toByteVector)
  }

  def hashJoinsChannel[C](channel: C, serializeC: Serialize[C]): Blake2b256Hash = {
    val cc = serializeC.toSizeHeadCodec
    hashWithSuffix(cc.encode(channel).getUnsafe, joinSuffixBits)

    // TODO: preparation for hard fork refactoring (direct use of Serialize[C])
    // hashWithSuffix(serializeC.encode(channel).toBitVector, joinSuffixBits)
  }

  def hashContinuationsChannels[C](channels: Seq[C], serializeC: Serialize[C]): Blake2b256Hash = {
    // Serialize[C] => Codec[C]
    val codecC = serializeC.toSizeHeadCodec
    // Codec[C] => Serialize[C]
    val serializeC2 = fromCodec(codecC)
    val chs         = toOrderedByteVectors(channels)(serializeC2)

    // TODO: preparation for hard fork refactoring (direct use of Serialize[C])
    // val chs = toOrderedByteVectors(channels)(serializeC)
    val channelsBits = codecSeqByteVector.encode(chs).getUnsafe
    hashWithSuffix(channelsBits, continuationSuffixBits)
  }

  // HACK to get single unforgeable name hash
  def getUnforgHash[C](channel: C) = {
    val par = channel.asInstanceOf[Par]
    par match {
      case p: Par =>
        if (p.unforgeables.size == 1) {
          // Single unforgeable name
          val unforg = p.unforgeables.head
          if (unforg.unfInstance.isDefined && unforg.unfInstance.isGPrivateBody) {
            val bs = unforg.unfInstance.gPrivateBody.get.id
            if (bs.size == 32) {
              // println(s"UNFORGEABLE NAME: size: ${bs.size}")
              Blake2b256Hash.fromByteArray(bs.toByteArray).some
            } else none
          } else none
        } else none
      case _ =>
        println(s"ERROR: Channel is not Par !!!!")
        none
    }
  }

  def hashDataChannel[C](channel: C, serializeC: Serialize[C]): Blake2b256Hash = {
    val cc = serializeC.toSizeHeadCodec

    // hashWithSuffix(cc.encode(channel).getUnsafe, dataSuffixBits)

    val bytesOpt = getUnforgHash(channel)
    bytesOpt.getOrElse(
      hashWithSuffix(cc.encode(channel).getUnsafe, dataSuffixBits)
    )

    // TODO: preparation for hard fork refactoring (direct use of Serialize[C])
    // hashWithSuffix(serializeC.encode(channel).toBitVector, dataSuffixBits)
  }

  /**
    * This function is needed just to support strange encoding of continuations in [[hashContinuationsChannels]].
    */
  private def fromCodec[A](codec: Codec[A]): Serialize[A] = new Serialize[A] {
    def encode(a: A): ByteVector = codec.encode(a).require.bytes
    def decode(bytes: ByteVector): Either[Throwable, A] =
      codec
        .decode(bytes.toBitVector)
        .toEither
        .fold(err => Left(new Exception(err.message)), v => Right(v.value))
  }
}
