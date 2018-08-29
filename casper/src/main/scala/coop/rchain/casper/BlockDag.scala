package coop.rchain.casper

import coop.rchain.casper.BlockDag.LatestMessages
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

import scala.collection.immutable.{HashMap, HashSet}

final case class BlockDag(idToBlocks: Map[Int, BlockMessage],
                          childMap: Map[BlockHash, Set[BlockHash]],
                          latestMessages: Map[Validator, BlockMessage],
                          latestMessagesOfLatestMessages: Map[Validator, LatestMessages],
                          currentId: Int,
                          currentSeqNum: Map[Validator, Int])

object BlockDag {
  type LatestMessages = Map[Validator, BlockHash]
  object LatestMessages {
    def empty: LatestMessages = HashMap.empty[Validator, BlockHash]
  }

  def apply(): BlockDag =
    new BlockDag(
      HashMap.empty[Int, BlockMessage],
      HashMap.empty[BlockHash, HashSet[BlockHash]],
      HashMap.empty[BlockHash, BlockMessage],
      HashMap.empty[Validator, LatestMessages],
      0,
      HashMap.empty[Validator, Int]
    )
}
