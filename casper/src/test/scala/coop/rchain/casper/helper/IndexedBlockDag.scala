package coop.rchain.casper.helper

import coop.rchain.casper.BlockDag
import coop.rchain.casper.BlockDag.LatestMessages
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

case class IndexedBlockDag(dag: BlockDag, idToBlocks: Map[Int, BlockMessage], currentId: Int) {
  def childMap: Map[BlockHash, Set[BlockHash]]     = dag.childMap
  def latestMessages: Map[Validator, BlockMessage] = dag.latestMessages
  def latestMessagesOfLatestMessages: Map[Validator, LatestMessages] =
    dag.latestMessagesOfLatestMessages
  def currentSeqNum: Map[Validator, Int] = dag.currentSeqNum

  def withLatestMessages(latestMessages: Map[Validator, BlockMessage]): IndexedBlockDag =
    this.copy(dag = dag.copy(latestMessages = latestMessages))
}

object IndexedBlockDag {
  def empty: IndexedBlockDag = IndexedBlockDag(BlockDag.empty, Map.empty[Int, BlockMessage], 0)

  def apply(idToBlocks: Map[Int, BlockMessage],
            childMap: Map[BlockHash, Set[BlockHash]],
            latestMessages: Map[Validator, BlockMessage],
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
