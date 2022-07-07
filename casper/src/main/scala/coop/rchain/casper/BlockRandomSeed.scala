package coop.rchain.casper

import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.RhoType.Name
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector
import scodec.codecs.{bytes, uint8, utf8, variableSizeBytes, vlong}
import scodec.{Codec, TransformSyntax}

import scala.collection.compat.immutable.LazyList

final case class BlockRandomSeed private (
    shardId: String,
    blockNumber: Long,
    sender: PublicKey,
    preStateHash: Blake2b256Hash
)

object BlockRandomSeed {
  // When deploying the user deploy , the chain would execute prechargeDeploy, userDeploy and RefundDeploy in
  // sequence. The split index for the random seed is based on the index of the executions.
  val PreChargeSplitIndex: Byte  = 0.toByte
  val UserDeploySplitIndex: Byte = 1.toByte
  val RefundSplitIndex: Byte     = 2.toByte

  /**
    * Creates a random seed for the random generator used to execute deploys in the block.
    */
  def apply(
      shardId: String,
      blockNumber: Long,
      sender: PublicKey,
      preStateHash: Blake2b256Hash
  ): BlockRandomSeed = {
    assert(shardId.onlyAscii, "Shard name should contain only ASCII characters")
    new BlockRandomSeed(shardId, blockNumber, sender, preStateHash)
  }

  /**
    * Special constructor which requires only shard ID. Used for genesis block so we are able to predict
    * unforgeable names in system contracts like mergeable tag in NonNegativeNumber contract.
    */
  def apply(shardId: String): BlockRandomSeed =
    apply(
      shardId,
      blockNumber = 0L,
      sender = PublicKey(Array[Byte]()),
      Blake2b256Hash.create(Array[Byte]())
    )

  /**
    * Special constructor which creates a random seed from the block data.
    */
  def apply(block: BlockMessage): BlockRandomSeed =
    if (block.justifications.isEmpty) {
      // Genesis block has no justifications
      apply(block.shardId)
    } else {
      apply(
        block.shardId,
        block.blockNumber,
        PublicKey(block.sender),
        block.preStateHash.toBlake2b256Hash
      )
    }

  /* Scodec definition used to get binary representation for the random seed */

  private val codecPublicKey: Codec[PublicKey] = variableSizeBytes(uint8, bytes)
    .xmap[PublicKey](bv => PublicKey(bv.toArray), pk => ByteVector(pk.bytes))

  val codecBlockRandomSeed: Codec[BlockRandomSeed] =
    (variableSizeBytes(uint8, utf8) :: vlong :: codecPublicKey :: Blake2b256Hash.codecBlake2b256Hash)
      .as[BlockRandomSeed]

  private def encode(blockRandomSeed: BlockRandomSeed): Array[Byte] =
    codecBlockRandomSeed.encode(blockRandomSeed).require.toByteArray

  /* Helper functions to create a random generator using different seed */

  /**
    * Creates random generator from the seed data.
    */
  def randomGenerator(
      shardId: String,
      blockNumber: Long,
      sender: PublicKey,
      preStateHash: Blake2b256Hash
  ): Blake2b512Random =
    randomGenerator(BlockRandomSeed(shardId, blockNumber, sender, preStateHash))

  /**
    * Creates a random generator from [[BlockRandomSeed]].
    */
  def randomGenerator(blockRandomSeed: BlockRandomSeed): Blake2b512Random =
    Blake2b512Random(encode(blockRandomSeed))

  /**
    * Creates a random generator just from shard ID, used for genesis block.
    */
  def randomGenerator(shardId: String): Blake2b512Random =
    randomGenerator(BlockRandomSeed(shardId))

  /**
    * Creates a random generator from data in the block. Valid for genesis block also.
    */
  def randomGenerator(block: BlockMessage): Blake2b512Random =
    randomGenerator(BlockRandomSeed(block))

  def splitRandomNumberFromGenesis(shardId: String, index: Byte, index2: Byte): Blake2b512Random = {
    val seed = randomGenerator(BlockRandomSeed(shardId))
    seed.splitByte(index).splitByte(index2)
  }

  /* Helper functions to create unforgeable names with random generator using specific seed */

  def nonNegativeMergeableTagName(
      shardId: String
  ): Par = {
    // NonNegative contract is the 4th contract deployed in the genesis, start from 0. Index should be 3
    val nonNegativeContractIndex: Byte = 3
    val rand                           = splitRandomNumberFromGenesis(shardId, nonNegativeContractIndex, UserDeploySplitIndex)
    val unforgeableByte                = Iterator.continually(rand.next()).drop(1).next()
    Name(unforgeableByte)
  }

  // This is the unforgeable name for
  // https://github.com/rchain/rchain/blob/43257ddb7b2b53cffb59a5fe1d4c8296c18b8292/casper/src/main/resources/RevVault.rho#L25
  def transferUnforgeable(shardId: String): Par = {
    // RevVault contract is the 7th contract deployed in the genesis, start from 0. Index should be 6
    val RevVaultContractDeployIndex: Byte = 6
    val rand =
      splitRandomNumberFromGenesis(shardId, RevVaultContractDeployIndex, UserDeploySplitIndex)
    val unfogeableBytes = Iterator.continually(rand.next()).drop(10).next()
    Name(unfogeableBytes)
  }

  def storeTokenUnforgeable(shardId: String): Par = {
    // TreeHashMap contract is the 1st contract deployed in the genesis, start from 0. Index should be 0
    val TreeHashMapContractDeployIndex: Byte = 0
    val rand =
      splitRandomNumberFromGenesis(shardId, TreeHashMapContractDeployIndex, UserDeploySplitIndex)
    val target = LazyList.continually(rand.next()).drop(9).head
    Name(target)
  }

  def revVaultUnforgeable(shardId: String): Par = {
    // RevVault contract is the 7th contract deployed in the genesis, start from 0. Index should be 6
    val RevVaultContractDeployIndex: Byte = 6
    val rand =
      splitRandomNumberFromGenesis(shardId, RevVaultContractDeployIndex, UserDeploySplitIndex)
    val unfogeableBytes = rand.next()
    Name(unfogeableBytes)
  }

}
