package coop.rchain.casper

import coop.rchain.blockstorage.dag.DagRepresentation
import coop.rchain.casper.protocol._
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator

/**
  * Casper snapshot is a state that is changing in discrete manner with each new block added.
  * This class represents full information about the state. It is required for creating new blocks
  * as well as for validating blocks.
  */
final case class CasperSnapshot(
    dag: DagRepresentation,
    fringe: Seq[BlockHash],
    lca: BlockHash,
    tips: IndexedSeq[BlockHash],
    justifications: Set[BlockMetadata],
    deploysInScope: Set[Signed[DeployData]],
    maxBlockNum: Long,
    maxSeqNums: Map[Validator, Long],
    onChainState: OnChainCasperState
)

final case class OnChainCasperState(
    shardConf: CasperShardConf,
    bondsMap: Map[Validator, Long],
    activeValidators: Seq[Validator]
)

final case class CasperShardConf(
    faultToleranceThreshold: Float,
    shardName: String,
    finalizationRate: Int,
    maxNumberOfParents: Int,
    maxParentDepth: Int,
    synchronyConstraintThreshold: Float,
    heightConstraintThreshold: Long,
    // Validators will try to put deploy in a block only for next `deployLifespan` blocks.
    // Required to enable protection from re-submitting duplicate deploys
    deployLifespan: Int,
    blockVersion: Int,
    configVersion: Long,
    bondMinimum: Long,
    bondMaximum: Long,
    epochLength: Int,
    quarantineLength: Int,
    minPhloPrice: Long
)
