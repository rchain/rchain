package coop.rchain.blockstorage

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.util.BlockMessageUtil
import coop.rchain.casper.protocol.{BlockMessage, Justification}

final case class BlockMetadata(
    blockHash: BlockHash,
    parents: List[BlockHash],
    sender: ByteString,
    justifications: List[Justification],
    weightMap: Map[ByteString, Long],
    seqNum: Int
)

object BlockMetadata {
  type Lookup = Map[BlockHash, BlockMetadata]

  def fromBlock(b: BlockMessage): BlockMetadata = BlockMetadata(
    b.blockHash,
    b.header.fold(List.empty[BlockHash])(_.parentsHashList.toList),
    b.sender,
    b.justifications.toList,
    BlockMessageUtil.weightMap(b),
    b.seqNum
  )
}
