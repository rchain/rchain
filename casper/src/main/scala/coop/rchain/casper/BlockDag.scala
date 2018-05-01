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
