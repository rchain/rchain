package coop.rchain.blockstorage

import java.nio.ByteBuffer

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.util.BlockMessageUtil
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.casper.protocol.{BlockMessage, Justification}

final case class BlockMetadata(
    blockHash: BlockHash,
    parents: List[BlockHash],
    sender: ByteString,
    justifications: List[Justification],
    weightMap: Map[Validator, Long],
    blockNum: Long,
    seqNum: Int
) {
  def toByteString: ByteString = {
    val blockMetadataSize =
      32 +
        4 + parents.size * 32 +
        32 +
        4 + justifications.size * (32 + 32) +
        4 + weightMap.size * (32 + 8) +
        8 +
        4
    val byteBuffer = ByteBuffer.allocate(blockMetadataSize)
    byteBuffer.put(blockHash.toByteArray)
    byteBuffer.putInt(parents.size)
    parents.foreach(parent => byteBuffer.put(parent.toByteArray))
    byteBuffer.put(sender.toByteArray)
    byteBuffer.putInt(justifications.size)
    justifications.foreach {
      case Justification(validator, latestBlockHash) =>
        byteBuffer.put(validator.toByteArray)
        byteBuffer.put(latestBlockHash.toByteArray)
    }
    byteBuffer.putInt(weightMap.size)
    weightMap.foreach {
      case (validator, stake) =>
        byteBuffer.put(validator.toByteArray)
        byteBuffer.putLong(stake)
    }
    byteBuffer.putLong(blockNum)
    byteBuffer.putInt(seqNum)
    ByteString.copyFrom(byteBuffer.array())
  }
}

object BlockMetadata {
  type Lookup = Map[BlockHash, BlockMetadata]

  def fromBlock(b: BlockMessage): BlockMetadata = BlockMetadata(
    b.blockHash,
    b.header.fold(List.empty[BlockHash])(_.parentsHashList.toList),
    b.sender,
    b.justifications.toList,
    BlockMessageUtil.weightMap(b),
    BlockMessageUtil.blockNumber(b),
    b.seqNum
  )

  def fromBytes(bytes: Array[Byte]): BlockMetadata = {
    val byteBuffer        = ByteBuffer.wrap(bytes)
    val blockHash         = byteBuffer.getBlockHash()
    val parentsSize       = byteBuffer.getInt()
    val parents           = (0 until parentsSize).map(_ => byteBuffer.getBlockHash()).toList
    val sender            = byteBuffer.getBlockHash()
    val justificationSize = byteBuffer.getInt()
    val justifications = (0 until justificationSize).map { _ =>
      val validator       = byteBuffer.getValidator()
      val latestBlockHash = byteBuffer.getBlockHash()
      Justification(validator, latestBlockHash)
    }.toList
    val weightMapSize = byteBuffer.getInt()
    val weightMap = (0 until weightMapSize).map { _ =>
      val validator = byteBuffer.getValidator()
      val stake     = byteBuffer.getLong()
      (validator, stake)
    }.toMap
    val blockNum = byteBuffer.getLong()
    val seqNum   = byteBuffer.getInt()
    BlockMetadata(
      blockHash,
      parents,
      sender,
      justifications,
      weightMap,
      blockNum,
      seqNum
    )
  }
}
