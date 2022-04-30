package coop.rchain.casper.engine

import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.Genesis.createGenesisBlock
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator}
import coop.rchain.casper.protocol.{CommUtil, _}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.metrics.Metrics
import coop.rchain.shared._

import java.nio.file.Path
import scala.concurrent.duration._

object ApproveBlockProtocol {

  def buildGenesis[F[_]: Concurrent: ContextShift: CommUtil: Log: EventLog: Time: Metrics: RuntimeManager: LastApprovedBlock](
      bondsPath: String,
      autogenShardSize: Int,
      genesisPath: Path,
      vaultsPath: String,
      minimumBond: Long,
      maximumBond: Long,
      epochLength: Int,
      quarantineLength: Int,
      numberOfActiveValidators: Int,
      shardId: String,
      deployTimestamp: Option[Long],
      requiredSigs: Int,
      duration: FiniteDuration,
      interval: FiniteDuration,
      blockNumber: Long,
      posMultiSigPublicKeys: List[String],
      posMultiSigQuorum: Int
  ): F[BlockMessage] =
    for {
      now       <- Time[F].currentMillis
      timestamp = deployTimestamp.getOrElse(now)

      vaults <- VaultParser.parse[F](vaultsPath)
      bonds <- BondsParser.parse[F](
                bondsPath,
                autogenShardSize
              )

      genesisBlock <- if (bonds.size <= requiredSigs)
                       Sync[F].raiseError[BlockMessage](
                         new Exception(
                           "Required sigs must be smaller than the number of bonded validators"
                         )
                       )
                     else {
                       val validators = bonds.toSeq.map(Validator.tupled)
                       createGenesisBlock(
                         implicitly[RuntimeManager[F]],
                         Genesis(
                           shardId = shardId,
                           timestamp = timestamp,
                           proofOfStake = ProofOfStake(
                             minimumBond = minimumBond,
                             maximumBond = maximumBond,
                             epochLength = epochLength,
                             quarantineLength = quarantineLength,
                             numberOfActiveValidators = numberOfActiveValidators,
                             validators = validators,
                             posMultiSigPublicKeys = posMultiSigPublicKeys,
                             posMultiSigQuorum = posMultiSigQuorum
                           ),
                           vaults = vaults,
                           supply = Long.MaxValue,
                           blockNumber = blockNumber
                         )
                       )
                     }

    } yield genesisBlock
}
