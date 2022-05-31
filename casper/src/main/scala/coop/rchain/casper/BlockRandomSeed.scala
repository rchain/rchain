package coop.rchain.casper

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector
import scodec.codecs.{bytes, uint8, ulong, utf8, variableSizeBytes}
import scodec.{Codec, TransformSyntax}

final case class BlockRandomSeed(
    shardId: String,
    blockNumber: Long,
    validatorPublicKey: PublicKey,
    preStateHash: Blake2b256Hash
)

object BlockRandomSeed {
  private val codecPublicKey: Codec[PublicKey] = variableSizeBytes(uint8, bytes)
    .xmap[PublicKey](bv => PublicKey(bv.toArray), pk => ByteVector(pk.bytes))

  private val codecBlockRandomSeed: Codec[BlockRandomSeed] = (utf8 :: ulong(bits = 64) ::
    codecPublicKey :: Blake2b256Hash.codecPureBlake2b256Hash).as[BlockRandomSeed]

  private def encode(blockRandomSeed: BlockRandomSeed): Array[Byte] =
    codecBlockRandomSeed.encode(blockRandomSeed).require.toByteArray

  def generateRandomNumber(blockRandomSeed: BlockRandomSeed): Blake2b512Random =
    Blake2b512Random(encode(blockRandomSeed))

  val PreChargeSplitIndex: Byte  = 1.toByte
  val UserDeploySplitIndex: Byte = 2.toByte
  val RefundSplitIndex: Byte     = 3.toByte
}
