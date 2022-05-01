package coop.rchain.casper.engine

import cats.effect.{Concurrent, ContextShift}
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

object ApproveBlockProtocol {

  def buildGenesis[F[_]: Concurrent: ContextShift: CommUtil: Log: EventLog: Time: Metrics: RuntimeManager: LastApprovedBlock](
      bondsPath: String,
      autogenShardSize: Int,
      vaultsPath: String,
      minimumBond: Long,
      maximumBond: Long,
      epochLength: Int,
      quarantineLength: Int,
      numberOfActiveValidators: Int,
      shardId: String,
      blockNumber: Long,
      posMultiSigPublicKeys: List[String],
      posMultiSigQuorum: Int
  ): F[BlockMessage] =
    for {
      timestamp <- Time[F].currentMillis

      vaults <- VaultParser.parse[F](vaultsPath)
      bonds <- BondsParser.parse[F](
                bondsPath,
                autogenShardSize
              )

      validators = bonds.toSeq.map(Validator.tupled)
      genesisBlock <- createGenesisBlock(
                       Genesis(
                         shardId = shardId,
                         blockTimestamp = timestamp,
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

    } yield genesisBlock
}
