package coop.rchain.casper

import coop.rchain.casper.BlockRandomSeed.encode
import coop.rchain.casper.protocol.BlockMessage
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
) {
  def generateRandomNumber: Blake2b512Random =
    Blake2b512Random(encode(this))
}

object BlockRandomSeed {
  private val codecPublicKey: Codec[PublicKey] = variableSizeBytes(uint8, bytes)
    .xmap[PublicKey](bv => PublicKey(bv.toArray), pk => ByteVector(pk.bytes))

  private val codecBlockRandomSeed: Codec[BlockRandomSeed] = (utf8 :: ulong(bits = 63) ::
    codecPublicKey :: Blake2b256Hash.codecPureBlake2b256Hash).as[BlockRandomSeed]

  private def encode(blockRandomSeed: BlockRandomSeed): Array[Byte] =
    codecBlockRandomSeed.encode(blockRandomSeed).require.toByteArray

  def fromBlock(block: BlockMessage): Blake2b512Random =
    BlockRandomSeed(
      block.shardId,
      block.body.state.blockNumber,
      PublicKey(block.sender),
      Blake2b256Hash.fromByteString(block.body.state.preStateHash)
    ).generateRandomNumber

  val PreChargeSplitIndex: Byte  = 1.toByte
  val UserDeploySplitIndex: Byte = 2.toByte
  val RefundSplitIndex: Byte     = 3.toByte
}
