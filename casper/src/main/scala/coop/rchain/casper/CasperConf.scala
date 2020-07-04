package coop.rchain.casper

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

final case class CasperConf(
    validatorPublicKey: Option[String],
    validatorPrivateKey: Option[String],
    validatorPrivateKeyPath: Option[Path],
    casperLoopInterval: FiniteDuration,
    requestedBlocksTimeout: FiniteDuration,
    forkChoiceStaleThreshold: FiniteDuration,
    forkChoiceCheckIfStaleInterval: FiniteDuration,
    roundRobinDispatcher: RoundRobinDispatcher,
    genesisBlockData: GenesisBlockData,
    genesisCeremony: GenesisCeremonyConf
)

final case class GenesisBlockData(
    genesisDataDir: Path,
    bondsFile: Option[String],
    walletsFile: Option[String],
    numberOfActiveValidators: Int,
    deployTimestamp: Option[Long],
    shardConfig: CasperShardConf
)

final case class GenesisCeremonyConf(
    requiredSignatures: Int,
    approveInterval: FiniteDuration,
    approveDuration: FiniteDuration,
    autogenShardSize: Int,
    genesisValidatorMode: Boolean,
    ceremonyMasterMode: Boolean
)

final case class RoundRobinDispatcher(
    maxPeerQueueSize: Int,
    giveUpAfterSkipped: Int,
    dropPeerAfterRetries: Int
)

final case class CasperShardConf(
    faultToleranceThreshold: Float,
    shardName: String,
    parentShardId: String,
    finalizationRate: Int,
    maxNumberOfParents: Int,
    maxParentDepth: Int,
    synchronyConstraintThreshold: Float,
    heightConstraintThreshold: Long,
    // Validators will try to put deploy in a block only for next `deployLifespan` blocks.
    // Required to enable protection from re-submitting duplicate deploys
    deployLifespan: Int,
    casperVersion: Long,
    configVersion: Long,
    bondMinimum: Long,
    bondMaximum: Long,
    epochLength: Int,
    quarantineLength: Int
)
