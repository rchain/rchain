package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.Log

final class LastFinalizedHeightConstraintChecker[F[_]: Sync: LastFinalizedStorage: Log](
    heightConstraintThreshold: Long
) {
  def check(
      dag: BlockDagRepresentation[F],
      genesis: BlockMessage,
      validator: Validator
  ): F[Boolean] =
    for {
      lastFinalizedBlockHash <- LastFinalizedStorage[F].getOrElse(genesis.blockHash)
      lastFinalizedBlock     <- dag.lookupUnsafe(lastFinalizedBlockHash)
      latestMessageOpt       <- dag.latestMessage(validator)
      result <- latestMessageOpt match {
                 case Some(latestMessage) =>
                   val latestFinalizedHeight = lastFinalizedBlock.blockNum
                   val heightDifference      = latestMessage.blockNum - latestFinalizedHeight
                   Log[F].info(
                     s"Latest message is $heightDifference blocks ahead of the last finalized block"
                   ) >> (heightDifference <= heightConstraintThreshold).pure[F]
                 case None =>
                   Sync[F].raiseError[Boolean](
                     new IllegalStateException("Validator does not have a latest message")
                   )
               }
    } yield result
}

object LastFinalizedHeightConstraintChecker {
  def apply[F[_]](
      implicit ev: LastFinalizedHeightConstraintChecker[F]
  ): LastFinalizedHeightConstraintChecker[F] =
    ev

  def apply[F[_]: Sync: LastFinalizedStorage: BlockStore: Log](
      heightConstraintThreshold: Long
  ): LastFinalizedHeightConstraintChecker[F] =
    new LastFinalizedHeightConstraintChecker[F](heightConstraintThreshold)
}
