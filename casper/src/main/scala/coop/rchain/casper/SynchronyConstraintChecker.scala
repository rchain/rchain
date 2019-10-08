package coop.rchain.casper

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.models.Validator.Validator

final class SynchronyConstraintChecker[F[_]: Sync: BlockStore](
    synchronyConstraintThreshold: Double
) {
  private def calculateSeenSendersSince(
      lastProposed: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[Set[Validator]] =
    for {
      latestMessages <- dag.latestMessageHashes
      seenSendersSince = lastProposed.justifications.flatMap {
        case Justification(validator, latestBlockHash) =>
          if (validator != lastProposed.sender && latestMessages(validator) != latestBlockHash) {
            // Since we would have fetched missing justifications initially, it can only mean
            // that we have received at least one new block since then
            Some(validator)
          } else {
            None
          }
      }.toSet
    } yield seenSendersSince

  def check(
      dag: BlockDagRepresentation[F],
      runtimeManager: RuntimeManager[F],
      genesis: BlockMessage,
      validator: Validator
  ): F[Boolean] =
    dag.latestMessageHash(validator).flatMap {
      case Some(lastProposedBlockHash) if lastProposedBlockHash == genesis.blockHash =>
        // The node has not proposed any block yet and hence allowed to propose once
        true.pure[F]
      case Some(lastProposedBlockHash) =>
        for {
          lastProposedBlock <- ProtoUtil.getBlock[F](lastProposedBlockHash)
          // Guaranteed to be present since last proposed block was present
          seenSenders            <- calculateSeenSendersSince(lastProposedBlock, dag)
          lastProposedTuplespace = ProtoUtil.tuplespace(lastProposedBlock)
          bonds                  <- runtimeManager.computeBonds(lastProposedTuplespace)
          validatorWeightMap     = bonds.map(b => b.validator -> b.stake).toMap
          sendersWeight          = seenSenders.toList.flatMap(s => validatorWeightMap.get(s)).sum
          otherValidatorsWeight  = validatorWeightMap.values.sum - validatorWeightMap(validator)
        } yield sendersWeight.toDouble / otherValidatorsWeight >= synchronyConstraintThreshold
      case None =>
        Sync[F].raiseError[Boolean](
          new IllegalStateException("Validator does not have a latest message")
        )
    }
}

object SynchronyConstraintChecker {
  def apply[F[_]](implicit ev: SynchronyConstraintChecker[F]): SynchronyConstraintChecker[F] =
    ev

  def apply[F[_]: Sync: BlockStore](
      synchronyConstraintThreshold: Double
  ): SynchronyConstraintChecker[F] =
    new SynchronyConstraintChecker[F](synchronyConstraintThreshold)
}
