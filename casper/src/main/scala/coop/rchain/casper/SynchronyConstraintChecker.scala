package coop.rchain.casper

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log

final class SynchronyConstraintChecker[F[_]: Sync: BlockStore: Log]() {
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
      validator: Validator,
      synchronyConstraintThreshold: Float
  ): F[Boolean] =
    dag.latestMessageHash(validator).flatMap {
      case Some(lastProposedBlockHash) if lastProposedBlockHash == genesis.blockHash =>
        // The node has not proposed any block yet and hence allowed to propose once
        true.pure[F]
      case Some(lastProposedBlockHash) =>
        for {
          lastProposedBlock <- BlockStore[F].getUnsafe(lastProposedBlockHash)
          // Guaranteed to be present since last proposed block was present
          seenSenders            <- calculateSeenSendersSince(lastProposedBlock, dag)
          lastProposedTuplespace = ProtoUtil.postStateHash(lastProposedBlock)
          bonds                  <- runtimeManager.computeBonds(lastProposedTuplespace)
          activeValidators       <- runtimeManager.getActiveValidators(lastProposedTuplespace)
          validatorWeightMap = bonds
            .filter(b => activeValidators.contains(b.validator))
            .map(b => b.validator -> b.stake)
            .toMap
          sendersWeight = seenSenders.toList.flatMap(s => validatorWeightMap.get(s)).sum
          // This method can be called on readonly node or not active validator.
          // So map validator -> stake might not have key associated with the node,
          // that's why we need `getOrElse`
          otherValidatorsWeight = validatorWeightMap.values.sum - validatorWeightMap
            .getOrElse(validator, 0L)
          // If there is no other active validators, do not put any constraint (value = 1)
          synchronyConstraintValue = if (otherValidatorsWeight == 0) 1
          else
            sendersWeight.toDouble / otherValidatorsWeight
          _ <- Log[F].info(
                s"Seen ${seenSenders.size} senders with weight $sendersWeight out of total $otherValidatorsWeight " +
                  s"(${synchronyConstraintValue} out of $synchronyConstraintThreshold needed)"
              )
        } yield synchronyConstraintValue >= synchronyConstraintThreshold
      case None =>
        Sync[F].raiseError[Boolean](
          new IllegalStateException("Validator does not have a latest message")
        )
    }
}

object SynchronyConstraintChecker {
  def apply[F[_]](implicit ev: SynchronyConstraintChecker[F]): SynchronyConstraintChecker[F] =
    ev
}
