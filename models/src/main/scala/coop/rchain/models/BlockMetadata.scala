package coop.rchain.models

import com.google.protobuf
import coop.rchain.casper.protocol._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash

final case class BlockMetadata(
    blockHash: BlockHash,
    blockNum: Long,
    sender: Validator,
    seqNum: Long,
    justifications: List[BlockHash],
    bondsMap: Map[Validator, Long],
    validated: Boolean,
    invalid: Boolean,
    finalized: Boolean,
    fringe: List[BlockHash],
    fringeStateHash: StateHash
)

object BlockMetadata {
  def from(b: BlockMetadataProto) = BlockMetadata(
    b.blockHash,
    b.blockNum,
    b.sender,
    b.seqNum,
    b.justifications,
    b.bonds.map(b => b.validator -> b.stake).toMap,
    b.validated,
    b.invalid,
    b.finalized,
    b.fringe,
    b.fringeStateHash
  )

  def toProto(b: BlockMetadata) = BlockMetadataProto(
    b.blockHash,
    b.blockNum,
    b.sender,
    b.seqNum,
    b.justifications,
    b.bondsMap.map { case (validator, stake) => BondProto(validator, stake) }.toList,
    b.validated,
    b.invalid,
    b.finalized,
    b.fringe,
    b.fringeStateHash
  )

  def fromBytes(bytes: Array[Byte]): BlockMetadata =
    from(BlockMetadataProto.parseFrom(bytes))

  def toBytes(b: BlockMetadata) = BlockMetadata.toProto(b).toByteArray

  def fromBlock(b: BlockMessage): BlockMetadata =
    BlockMetadata(
      b.blockHash,
      b.blockNumber,
      b.sender,
      b.seqNum,
      b.justifications,
      b.bonds,
      validated = false,
      invalid = false,
      finalized = false,
      fringe = List(),
      fringeStateHash = protobuf.ByteString.EMPTY
    )
}
