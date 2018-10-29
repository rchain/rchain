package coop.rchain.casper.helper

import coop.rchain.blockstorage.BlockMetadata
import coop.rchain.casper.BlockDag
import coop.rchain.casper.BlockDag.LatestMessages
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

case class IndexedBlockDag(dag: BlockDag, idToBlocks: Map[Int, BlockMessage], currentId: Int) {
  def childMap: Map[BlockHash, Set[BlockHash]]     = dag.childMap
  def latestMessages: Map[Validator, BlockMessage] = dag.latestMessages
  def latestMessagesOfLatestMessages: Map[Validator, LatestMessages] =
    dag.latestMessagesOfLatestMessages
  def dataLookup: BlockMetadata.Lookup    = dag.dataLookup
  def topoSort: Vector[Vector[BlockHash]] = dag.topoSort
  def sortOffset: Long                    = dag.sortOffset

  def withLatestMessages(latestMessages: Map[Validator, BlockMessage]): IndexedBlockDag =
    this.copy(dag = dag.copy(latestMessages = latestMessages))

  def withOffset(offset: Long): IndexedBlockDag =
    this.copy(dag = dag.copy(sortOffset = offset))
}

object IndexedBlockDag {
  def empty: IndexedBlockDag = IndexedBlockDag(BlockDag.empty, Map.empty[Int, BlockMessage], 0)

  def apply(
      idToBlocks: Map[Int, BlockMessage],
      childMap: Map[BlockHash, Set[BlockHash]],
      latestMessages: Map[Validator, BlockMessage],
      latestMessagesOfLatestMessages: Map[Validator, LatestMessages],
      currentId: Int,
      dataLookup: BlockMetadata.Lookup,
      topoSort: Vector[Vector[BlockHash]],
      sortOffset: Long
  ): IndexedBlockDag = IndexedBlockDag(
    BlockDag(
      childMap,
      latestMessages,
      latestMessagesOfLatestMessages,
      dataLookup,
      topoSort,
      sortOffset
    ),
    idToBlocks,
    currentId
  )
}
