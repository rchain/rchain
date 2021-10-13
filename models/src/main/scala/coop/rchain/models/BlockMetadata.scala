package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.models.BlockHash.BlockHash
import scalapb.TypeMapper

final case class BlockMetadata(
    blockHash: ByteString,
    postStateHash: ByteString,
    sender: ByteString,
    justifications: List[Justification],
    parents: List[BlockHash],
    weightMap: Map[ByteString, Long],
    blockNum: Long,
    seqNum: Int,
    invalid: Boolean,
    stateMetadata: StateMetadata
) {
  def toByteString = BlockMetadata.typeMapper.toBase(this).toByteString
}

object BlockMetadata {

  implicit val typeMapper = TypeMapper[BlockMetadataInternal, BlockMetadata] { internal =>
    BlockMetadata(
      internal.blockHash,
      internal.postStateHash,
      internal.sender,
      internal.justifications.map(Justification.from),
      internal.parents,
      internal.bonds.map(b => b.validator -> b.stake).toMap,
      internal.blockNum,
      internal.seqNum,
      internal.invalid,
      internal.stateMetadata.get
    )
  } { metadata =>
    BlockMetadataInternal(
      metadata.blockHash,
      metadata.postStateHash,
      metadata.sender,
      metadata.justifications.map(Justification.toProto),
      metadata.parents,
      metadata.weightMap.map { case (validator, stake) => BondProto(validator, stake) }.toList,
      metadata.blockNum,
      metadata.seqNum,
      metadata.invalid,
      Some(metadata.stateMetadata)
    )
  }

  private val byteStringOrdering =
    Ordering.by[ByteString, Iterable[Byte]](_.toByteArray)(Ordering.Iterable[Byte])

  val orderingByNum: Ordering[BlockMetadata] =
    (l: BlockMetadata, r: BlockMetadata) => {
      l.blockNum.compare(r.blockNum) match {
        case 0 => byteStringOrdering.compare(l.blockHash, r.blockHash)
        case v => v
      }
    }

  def fromBytes(bytes: Array[Byte]): BlockMetadata =
    typeMapper.toCustom(BlockMetadataInternal.parseFrom(bytes))

  private def weightMap(state: RChainState): Map[ByteString, Long] =
    state.bonds.map {
      case Bond(validator, stake) => validator -> stake
    }.toMap

  def fromBlock(
      b: BlockMessage,
      invalid: Boolean,
      stateMetadata: StateMetadata
  ): BlockMetadata =
    BlockMetadata(
      b.blockHash,
      b.body.state.postStateHash,
      b.sender,
      b.justifications,
      List(), // This is computed and filled when inserting to storage
      weightMap(b.body.state),
      b.body.state.blockNumber,
      b.seqNum,
      invalid,
      stateMetadata
    )
}
