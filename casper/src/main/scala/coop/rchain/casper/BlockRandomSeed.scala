package coop.rchain.casper

import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.RuntimeManager.emptyStateHashFixed
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.models.syntax._
import scodec.bits.ByteVector
import scodec.codecs.{bytes, uint8, ulong, utf8, variableSizeBytes}
import scodec.{Codec, TransformSyntax}

final case class BlockRandomSeed private (
    shardId: String,
    blockNumber: Long,
    sender: PublicKey,
    preStateHash: Blake2b256Hash
)

object BlockRandomSeed {
  def apply(
      shardId: String,
      blockNumber: Long,
      sender: PublicKey,
      preStateHash: Blake2b256Hash
  ): BlockRandomSeed = new BlockRandomSeed(shardId, blockNumber, sender, preStateHash)

  private val codecPublicKey: Codec[PublicKey] = variableSizeBytes(uint8, bytes)
    .xmap[PublicKey](bv => PublicKey(bv.toArray), pk => ByteVector(pk.bytes))

  val codecBlockRandomSeed: Codec[BlockRandomSeed] =
    (variableSizeBytes(uint8, utf8) :: ulong(bits = 63) ::
      codecPublicKey :: Blake2b256Hash.codecBlake2b256Hash).as[BlockRandomSeed]

  private def encode(blockRandomSeed: BlockRandomSeed): Array[Byte] =
    codecBlockRandomSeed.encode(blockRandomSeed).require.toByteArray

  def generateRandomNumber(blockRandomSeed: BlockRandomSeed): Blake2b512Random =
    Blake2b512Random(encode(blockRandomSeed))

  def generateSplitRandomNumber(blockRandomSeed: BlockRandomSeed, index: Byte): Blake2b512Random =
    generateRandomNumber(blockRandomSeed).splitByte(index)

  def generateSplitRandomNumber(
      blockRandomSeed: BlockRandomSeed,
      index: Byte,
      index2: Byte
  ): Blake2b512Random =
    generateRandomNumber(blockRandomSeed).splitByte(index).splitByte(index2)

  def fromBlock(block: BlockMessage): Blake2b512Random = {
    val seed = BlockRandomSeed(
      block.shardId,
      block.blockNumber,
      PublicKey(block.sender),
      block.preStateHash.toBlake2b256Hash
    )
    generateRandomNumber(seed)
  }

  def fromGenesis(block: BlockMessage): Blake2b512Random = {
    val seed = BlockRandomSeed(
      block.shardId,
      Genesis.genesisRandomSeedBlockNumber,
      Genesis.genesisRandomSeedPubKey,
      emptyStateHashFixed.toBlake2b256Hash
    )
    generateRandomNumber(seed)
  }

  // When deploying the user deploy , the chain would execute prechargeDeploy, userDeploy and RefundDeploy in
  // sequence. The split index for the random seed is based on the index of the executions.
  val PreChargeSplitIndex: Byte  = 0.toByte
  val UserDeploySplitIndex: Byte = 1.toByte
  val RefundSplitIndex: Byte     = 2.toByte
}
