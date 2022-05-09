package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._

final case class BlockMetadata(
    blockHash: ByteString,
    blockNum: Long,
    sender: ByteString,
    seqNum: Int,
    justifications: List[Justification],
    weightMap: Map[ByteString, Long],
    invalid: Boolean,
    directlyFinalized: Boolean,
    finalized: Boolean,
    parents: List[ByteString]
)

object BlockMetadata {
  def from(b: BlockMetadataProto) = BlockMetadata(
    b.blockHash,
    b.blockNum,
    b.sender,
    b.seqNum,
    b.justifications.map(Justification.from),
    b.bonds.map(b => b.validator -> b.stake).toMap,
    b.invalid,
    b.directlyFinalized,
    b.finalized,
    b.parents
  )

  def toProto(b: BlockMetadata) = BlockMetadataProto(
    b.blockHash,
    b.blockNum,
    b.sender,
    b.seqNum,
    b.justifications.map(Justification.toProto),
    b.weightMap.map { case (validator, stake) => BondProto(validator, stake) }.toList,
    b.invalid,
    b.directlyFinalized,
    b.finalized,
    b.parents
  )

  def fromBytes(bytes: Array[Byte]): BlockMetadata =
    from(BlockMetadataProto.parseFrom(bytes))

  def toBytes(b: BlockMetadata) = BlockMetadata.toProto(b).toByteArray

  private def weightMap(state: RChainState): Map[ByteString, Long] =
    state.bonds.map {
      case Bond(validator, stake) => validator -> stake
    }.toMap

  def fromBlock(
      b: BlockMessage,
      invalid: Boolean,
      directlyFinalized: Boolean = false,
      finalized: Boolean = false
  ): BlockMetadata =
    BlockMetadata(
      b.blockHash,
      b.body.state.blockNumber,
      b.sender,
      b.seqNum,
      b.justifications,
      weightMap(b.body.state),
      invalid,
      directlyFinalized,
      finalized,
      b.header.parentsHashList
    )
}
