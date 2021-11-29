package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.blocks.proposer.{
  CheckProposeConstraintsResult,
  TooFarAheadOfLastFinalized
}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.syntax._
import coop.rchain.shared.Log

final class LastFinalizedHeightConstraintChecker[F[_]: Sync: Log] {
  def check(
      s: CasperSnapshot[F],
      // TODO having genesis is a weird way to check, remove
      genesis: BlockMessage,
      validatorIdentity: ValidatorIdentity
  ): F[CheckProposeConstraintsResult] = {
    val validator                 = ByteString.copyFrom(validatorIdentity.publicKey.bytes)
    val heightConstraintThreshold = s.onChainState.shardConf.heightConstraintThreshold
    val lastFinalizedBlockHash    = s.dag.lastFinalizedBlock
    for {
      lastFinalizedBlock <- s.dag.lookupUnsafe(lastFinalizedBlockHash)
      latestMessageOpt   <- s.dag.latestMessage(validator)
      result <- latestMessageOpt match {
                 case Some(latestMessage) =>
                   val latestFinalizedHeight = lastFinalizedBlock.blockNum
                   val heightDifference      = latestMessage.blockNum - latestFinalizedHeight
                   val result =
                     if (heightDifference <= heightConstraintThreshold)
                       CheckProposeConstraintsResult.success
                     else TooFarAheadOfLastFinalized
                   Log[F].info(
                     s"Latest message is $heightDifference blocks ahead of the last finalized block"
                   ) >> result.pure[F]
                 case None =>
                   Sync[F].raiseError[CheckProposeConstraintsResult](
                     new IllegalStateException("Validator does not have a latest message")
                   )
               }
    } yield result
  }
}

object LastFinalizedHeightConstraintChecker {
  def apply[F[_]: Sync: BlockStore: Log]: LastFinalizedHeightConstraintChecker[F] =
    new LastFinalizedHeightConstraintChecker[F]
}
