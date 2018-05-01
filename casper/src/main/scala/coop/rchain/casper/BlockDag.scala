package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

import scala.collection.immutable.{HashMap, HashSet}

final case class BlockDag(idToBlocks: HashMap[Int, BlockMessage],
                          blockLookup: HashMap[ByteString, BlockMessage],
                          childMap: HashMap[BlockHash, HashSet[BlockHash]],
                          latestMessages: HashMap[Validator, BlockHash],
                          currentId: Int)

object BlockDag {
  def apply(idToBlocks: HashMap[Int, BlockMessage],
            blockLookup: HashMap[BlockHash, BlockMessage],
            childMap: HashMap[BlockHash, HashSet[BlockHash]],
            latestMessages: HashMap[Validator, BlockHash],
            currentId: Int): BlockDag = new BlockDag(idToBlocks, blockLookup, childMap, latestMessages, currentId)

  def apply(): BlockDag = new BlockDag(HashMap.empty[Int, BlockMessage], HashMap.empty[BlockHash, BlockMessage], HashMap.empty[BlockHash, HashSet[BlockHash]], HashMap.empty[Validator, BlockHash], 0)
}
