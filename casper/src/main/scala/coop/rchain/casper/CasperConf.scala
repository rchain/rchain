package coop.rchain.casper

import java.nio.file.Path
import scala.concurrent.duration.FiniteDuration

final case class CasperConf(
    validatorPublicKey: Option[String],
    validatorPrivateKey: Option[String],
    validatorPrivateKeyPath: Option[Path],
    shardName: String,
    casperLoopInterval: FiniteDuration,
    requestedBlocksTimeout: FiniteDuration,
    maxNumberOfParents: Int,
    forkChoiceStaleThreshold: FiniteDuration,
    forkChoiceCheckIfStaleInterval: FiniteDuration,
    synchronyConstraintThreshold: Double,
    heightConstraintThreshold: Long,
    genesisBlockData: GenesisBlockData,
    autogenShardSize: Int,
    minPhloPrice: Long
)

final case class GenesisBlockData(
    genesisDataDir: Path,
    bondsFile: String,
    walletsFile: String,
    bondMinimum: Long,
    bondMaximum: Long,
    epochLength: Int,
    quarantineLength: Int,
    genesisBlockNumber: Long,
    numberOfActiveValidators: Int,
    posMultiSigPublicKeys: List[String],
    posMultiSigQuorum: Int,
    posVaultPubKey: String,
    systemContractPubKey: String
)
