package coop.rchain.casper

import coop.rchain.blockstorage.BlockMetadata
import coop.rchain.casper.BlockDag.LatestMessages
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.BlockMessage

import scala.collection.immutable.{HashMap, HashSet}

/*
 * topoSort - Topological sort of the DAG. The index in the outer Vector
 *            is the block number (length of the longest path back to
 *            genesis) and the index of the inner Vector breaks ties for
 *            blocks with the same number based when the node observed the block.
 *            `topoSort.flatten` gives the sorted blocks.
 *
 * sortOffset - This allows us to sort from some block number going forward.
 *              The idea is that the block height may get too large to use as the index
 *              of a Vector, or that after a checkpoint on the chain we won't need
 *              to remember the sorting before that block. The offset gives the block
 *              number for the first block in the currently maintained sorting.
 */
final case class BlockDag(
    childMap: Map[BlockHash, Set[BlockHash]],
    latestMessages: Map[Validator, BlockMessage],
    latestMessagesOfLatestMessages: Map[Validator, LatestMessages],
    dataLookup: BlockMetadata.Lookup,
    topoSort: Vector[Vector[BlockHash]],
    sortOffset: Long
)

object BlockDag {
  type LatestMessages = Map[Validator, BlockHash]
  object LatestMessages {
    def empty: LatestMessages = HashMap.empty[Validator, BlockHash]
  }

  val empty: BlockDag =
    new BlockDag(
      HashMap.empty[BlockHash, HashSet[BlockHash]],
      HashMap.empty[BlockHash, BlockMessage],
      HashMap.empty[Validator, LatestMessages],
      HashMap.empty[BlockHash, BlockMetadata],
      Vector.empty[Vector[BlockHash]],
      0L
    )

  def deriveOrdering(dag: BlockDag): Ordering[BlockMetadata] = {
    val order = dag.topoSort.flatten.zipWithIndex.toMap
    Ordering.by(b => order(b.blockHash))
  }
}
