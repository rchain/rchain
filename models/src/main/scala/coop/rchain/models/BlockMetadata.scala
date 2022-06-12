package coop.rchain.models

import coop.rchain.casper.protocol._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator

final case class BlockMetadata(
    blockHash: BlockHash,
    blockNum: Long,
    sender: Validator,
    seqNum: Long,
    justifications: List[BlockHash],
    weightMap: Map[Validator, Long],
    invalid: Boolean,
    finalized: Boolean,
    parents: List[BlockHash]
)

object BlockMetadata {
  def from(b: BlockMetadataProto) = BlockMetadata(
    b.blockHash,
    b.blockNum,
    b.sender,
    b.seqNum,
    b.justifications,
    b.bonds.map(b => b.validator -> b.stake).toMap,
    b.invalid,
    b.finalized,
    b.parents
  )

  def toProto(b: BlockMetadata) = BlockMetadataProto(
    b.blockHash,
    b.blockNum,
    b.sender,
    b.seqNum,
    b.justifications,
    b.weightMap.map { case (validator, stake) => BondProto(validator, stake) }.toList,
    b.invalid,
    b.finalized,
    b.parents
  )

  def fromBytes(bytes: Array[Byte]): BlockMetadata =
    from(BlockMetadataProto.parseFrom(bytes))

  def toBytes(b: BlockMetadata) = BlockMetadata.toProto(b).toByteArray

  def fromBlock(
      b: BlockMessage,
      invalid: Boolean,
      finalized: Boolean = false
  ): BlockMetadata =
    BlockMetadata(
      b.blockHash,
      b.blockNumber,
      b.sender,
      b.seqNum,
      b.justifications,
      b.bonds,
      invalid,
      finalized,
      List()
    )
}
