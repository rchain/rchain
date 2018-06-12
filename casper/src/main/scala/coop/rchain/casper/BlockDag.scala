package coop.rchain.casper

import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

import scala.collection.immutable.{HashMap, HashSet}

final case class BlockDag(idToBlocks: Map[Int, BlockMessage],
                          blockLookup: Map[BlockHash, BlockMessage],
                          childMap: Map[BlockHash, Set[BlockHash]],
                          latestMessages: Map[Validator, BlockHash],
                          currentId: Int,
                          currentSeqNum: Map[Validator, Int])

object BlockDag {
  def apply(): BlockDag =
    new BlockDag(
      HashMap.empty[Int, BlockMessage],
      HashMap.empty[BlockHash, BlockMessage],
      HashMap.empty[BlockHash, HashSet[BlockHash]],
      HashMap.empty[Validator, BlockHash],
      0,
      HashMap.empty[Validator, Int]
    )
}
