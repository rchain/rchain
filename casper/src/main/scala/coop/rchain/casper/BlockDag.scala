package coop.rchain.casper

import coop.rchain.casper.BlockDag.LatestMessages
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

import scala.collection.immutable.{HashMap, HashSet}

final case class BlockDag(childMap: Map[BlockHash, Set[BlockHash]],
                          latestMessages: Map[Validator, BlockMessage.Safe],
                          latestMessagesOfLatestMessages: Map[Validator, LatestMessages],
                          currentSeqNum: Map[Validator, Int])

object BlockDag {
  type LatestMessages = Map[Validator, BlockHash]
  object LatestMessages {
    def empty: LatestMessages = HashMap.empty[Validator, BlockHash]
  }

  val empty: BlockDag =
    new BlockDag(
      HashMap.empty[BlockHash, HashSet[BlockHash]],
      HashMap.empty[BlockHash, BlockMessage.Safe],
      HashMap.empty[Validator, LatestMessages],
      HashMap.empty[Validator, Int]
    )
}
