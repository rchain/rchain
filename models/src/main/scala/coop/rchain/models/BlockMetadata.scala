package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import scalapb.TypeMapper

final case class BlockMetadata(
    blockHash: ByteString,
    parents: List[ByteString],
    sender: ByteString,
    justifications: List[Justification],
    weightMap: Map[ByteString, Long],
    blockNum: Long,
    seqNum: Int
) {
  def toByteString = BlockMetadata.typeMapper.toBase(this).toByteString
}

object BlockMetadata {
  implicit val typeMapper = TypeMapper[BlockMetadataInternal, BlockMetadata] { internal =>
    BlockMetadata(
      internal.blockHash,
      internal.parents,
      internal.sender,
      internal.justifications,
      internal.bonds.map(b => b.validator -> b.stake).toMap,
      internal.blockNum,
      internal.seqNum
    )
  } { metadata =>
    BlockMetadataInternal(
      metadata.blockHash,
      metadata.parents,
      metadata.sender,
      metadata.justifications,
      metadata.weightMap.map { case (validator, stake) => Bond(validator, stake) }.toList,
      metadata.blockNum,
      metadata.seqNum
    )
  }

  def fromBytes(bytes: Array[Byte]): BlockMetadata =
    typeMapper.toCustom(BlockMetadataInternal.parseFrom(bytes))

  private def weightMap(blockMessage: BlockMessage): Map[ByteString, Long] =
    blockMessage.body match {
      case Some(block) =>
        block.state match {
          case Some(state) => weightMap(state)
          case None        => Map.empty[ByteString, Long]
        }
      case None => Map.empty[ByteString, Long]
    }

  private def weightMap(state: RChainState): Map[ByteString, Long] =
    state.bonds.map {
      case Bond(validator, stake) => validator -> stake
    }.toMap

  def fromBlock(b: BlockMessage): BlockMetadata =
    BlockMetadata(
      b.blockHash,
      b.header.fold(List.empty[ByteString])(_.parentsHashList.toList),
      b.sender,
      b.justifications.toList,
      weightMap(b),
      b.body.flatMap(_.state.map(_.blockNumber)).getOrElse(0L),
      b.seqNum
    )
}
