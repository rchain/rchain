package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.casper.util.ProtoUtil

final case class BlockMetadata(
    blockHash: BlockHash,
    parents: List[BlockHash],
    sender: ByteString,
    justifications: List[Justification],
    weightMap: Map[ByteString, Long],
    blockNum: Long,
    seqNum: Int
)

object BlockMetadata {
  type Lookup = Map[BlockHash, BlockMetadata]

  def fromBlock(b: BlockMessage): BlockMetadata = BlockMetadata(
    b.blockHash,
    b.header.fold(List.empty[BlockHash])(_.parentsHashList.toList),
    b.sender,
    b.justifications.toList,
    ProtoUtil.weightMap(b),
    ProtoUtil.blockNumber(b),
    b.seqNum
  )
}
