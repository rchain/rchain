package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.blocks.proposer.{
  CheckProposeConstraintsResult,
  TooFarAheadOfLastFinalized
}
import coop.rchain.casper.syntax._
import coop.rchain.shared.Log

// TODO: add comments how it works and adjust for multi-parent finalization
final class LastFinalizedHeightConstraintChecker[F[_]: Sync: Log: BlockDagStorage] {
  def check(
      s: CasperSnapshot,
      validatorIdentity: ValidatorIdentity
  ): F[CheckProposeConstraintsResult] = {
    val validator                 = ByteString.copyFrom(validatorIdentity.publicKey.bytes)
    val heightConstraintThreshold = s.onChainState.shardConf.heightConstraintThreshold
    for {
      lastFinalizedBlock <- s.dag.lastFinalizedBlockHash.flatTraverse(s.dag.lookup(_))
      latestMessageOpt   <- s.dag.latestMessage(validator)
      result <- latestMessageOpt match {
                 case Some(latestMessage) =>
                   val latestFinalizedHeight = lastFinalizedBlock.map(_.blockNum).getOrElse(-1L)
                   val heightDifference      = latestMessage.blockNum - latestFinalizedHeight
                   val result =
                     if (heightDifference <= heightConstraintThreshold)
                       CheckProposeConstraintsResult.success
                     else TooFarAheadOfLastFinalized
                   Log[F].info(
                     s"Latest message is $heightDifference blocks ahead of the last finalized block"
                   ) >> result.pure[F]
                 case None =>
                   CheckProposeConstraintsResult.success.pure[F]
               }
    } yield result
  }
}

object LastFinalizedHeightConstraintChecker {
  def apply[F[_]: Sync: BlockStore: BlockDagStorage: Log]: LastFinalizedHeightConstraintChecker[F] =
    new LastFinalizedHeightConstraintChecker[F]
}
