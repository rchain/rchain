package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import scalapb.TypeMapper
import cats.syntax.all._
import coop.rchain.models.syntax.show

final case class BlockMetadata(
    blockHash: ByteString,
    postStateHash: ByteString,
    parents: List[ByteString],
    sender: ByteString,
    justifications: List[Justification],
    weightMap: Map[ByteString, Long],
    blockNum: Long,
    seqNum: Long,
    invalid: Boolean,
    baseFringeNum: Long,
    finalView: Map[ByteString, ByteString]
) {
  override def equals(obj: Any): Boolean = obj match {
    case that: BlockMetadata => that.blockHash == this.blockHash
    case _                   => false
  }
  override def hashCode(): Int = blockHash.hashCode()

  def toByteString = BlockMetadata.typeMapper.toBase(this).toByteString

  override def toString: String = s"${blockHash.show}(bn $blockNum | sn $seqNum)"
}

object BlockMetadata {
  implicit val typeMapper = TypeMapper[BlockMetadataInternal, BlockMetadata] { internal =>
    BlockMetadata(
      internal.blockHash,
      internal.postStateHash,
      internal.parents,
      internal.sender,
      internal.justifications.map(Justification.from),
      internal.bonds.map(b => b.validator -> b.stake).toMap,
      internal.blockNum,
      internal.seqNum,
      internal.invalid,
      internal.baseFringeNum,
      internal.finalView.map(b => b.validator -> b.hash).toMap
    )
  } { metadata =>
    BlockMetadataInternal(
      metadata.blockHash,
      metadata.parents,
      metadata.sender,
      metadata.justifications.map(Justification.toProto),
      metadata.weightMap.map { case (validator, stake) => BondProto(validator, stake) }.toList,
      metadata.blockNum,
      metadata.seqNum,
      metadata.invalid,
      metadata.postStateHash,
      metadata.baseFringeNum,
      metadata.finalView.map { case (validator, hash) => FringeItemProto(validator, hash) }.toList
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
      baseFringeNum: Long
  ): BlockMetadata =
    BlockMetadata(
      b.blockHash,
      b.body.state.postStateHash,
      b.header.parentsHashList,
      b.sender,
      b.justifications,
      weightMap(b.body.state),
      b.body.state.blockNumber,
      b.seqNum,
      invalid,
      baseFringeNum,
      Map()
    )
}
