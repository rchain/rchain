package coop.rchain.casper

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

final case class CasperConf(
    faultToleranceThreshold: Float,
    validatorPublicKey: Option[String],
    validatorPrivateKey: Option[String],
    validatorPrivateKeyPath: Option[Path],
    shardName: String,
    parentShardId: String,
    casperLoopInterval: FiniteDuration,
    requestedBlocksTimeout: FiniteDuration,
    finalizationRate: Int,
    maxNumberOfParents: Int,
    maxParentDepth: Option[Int],
    forkChoiceStaleThreshold: FiniteDuration,
    forkChoiceCheckIfStaleInterval: FiniteDuration,
    synchronyConstraintThreshold: Double,
    heightConstraintThreshold: Long,
    roundRobinDispatcher: RoundRobinDispatcher,
    genesisBlockData: GenesisBlockData,
    genesisCeremony: GenesisCeremonyConf
)

final case class GenesisBlockData(
    genesisDataDir: Path,
    bondsFile: Option[String],
    walletsFile: Option[String],
    bondMinimum: Long,
    bondMaximum: Long,
    epochLength: Int,
    quarantineLength: Int,
    numberOfActiveValidators: Int,
    deployTimestamp: Option[Long]
)

final case class GenesisCeremonyConf(
    requiredSignatures: Int,
    approveInterval: FiniteDuration,
    approveDuration: FiniteDuration,
    autogenShardSize: Int,
    genesisValidatorMode: Boolean,
    ceremonyMasterMode: Boolean,
    fromGenesis: Boolean
)

final case class RoundRobinDispatcher(
    maxPeerQueueSize: Int,
    giveUpAfterSkipped: Int,
    dropPeerAfterRetries: Int
)
