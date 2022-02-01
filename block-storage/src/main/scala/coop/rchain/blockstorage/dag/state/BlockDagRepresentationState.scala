package coop.rchain.blockstorage.dag.state

import coop.rchain.blockstorage.dag.BlockDagStorage.DagFringe
import coop.rchain.blockstorage.dag.state.BlockDagRepresentationState.BlockDagFinalizationState
import coop.rchain.casper.protocol.DeployChain
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator

import scala.collection.immutable.{Set, SortedMap}

/** Validated part of BlockDag */
final case class BlockDagRepresentationState(
    dagSet: Set[BlockHash],
    latestMessagesMap: Map[Validator, BlockHash],
    childrenMap: Map[BlockHash, Map[Validator, Vector[BlockHash]]],
    witnessMap: Map[BlockHash, Map[Validator, BlockHash]],
    heightMap: SortedMap[Long, Set[BlockHash]],
    invalidBlocksSet: Set[BlockHash],
    finalizationState: BlockDagFinalizationState,
    latestFringes: SortedMap[Long, DagFringe],
    finalityViews: Map[Validator, Long]
)

object BlockDagRepresentationState {
  final case class BlockDagFinalizationState(accepted: Set[DeployChain], rejected: Set[DeployChain])
}
