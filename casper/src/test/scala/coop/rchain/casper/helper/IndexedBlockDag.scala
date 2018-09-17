package coop.rchain.casper.helper

import coop.rchain.casper.BlockDag
import coop.rchain.casper.BlockDag.LatestMessages
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

case class IndexedBlockDag(dag: BlockDag, idToBlocks: Map[Int, BlockMessage.Safe], currentId: Int) {
  def childMap: Map[BlockHash, Set[BlockHash]]          = dag.childMap
  def latestMessages: Map[Validator, BlockMessage.Safe] = dag.latestMessages
  def latestMessagesOfLatestMessages: Map[Validator, LatestMessages] =
    dag.latestMessagesOfLatestMessages
  def currentSeqNum: Map[Validator, Int] = dag.currentSeqNum

  def withLatestMessages(latestMessages: Map[Validator, BlockMessage.Safe]): IndexedBlockDag =
    this.copy(dag = dag.copy(latestMessages = latestMessages))
}

object IndexedBlockDag {
  def empty: IndexedBlockDag = IndexedBlockDag(BlockDag.empty, Map.empty[Int, BlockMessage.Safe], 0)

  def apply(idToBlocks: Map[Int, BlockMessage.Safe],
            childMap: Map[BlockHash, Set[BlockHash]],
            latestMessages: Map[Validator, BlockMessage.Safe],
            latestMessagesOfLatestMessages: Map[Validator, LatestMessages],
            currentId: Int,
            currentSeqNum: Map[Validator, Int]): IndexedBlockDag = IndexedBlockDag(
    BlockDag(
      childMap,
      latestMessages,
      latestMessagesOfLatestMessages,
      currentSeqNum
    ),
    idToBlocks,
    currentId
  )
}
