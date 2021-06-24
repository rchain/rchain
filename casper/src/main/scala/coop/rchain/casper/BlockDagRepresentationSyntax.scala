package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.finality.Finalizer
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator

trait BlockDagRepresentationSyntax {
  implicit final def casperSyntaxBlockDagRepresentation[F[_]](
      dag: BlockDagRepresentation[F]
  ): BlockDagRepresentationOps[F] = new BlockDagRepresentationOps[F](dag)
}

final class BlockDagRepresentationOps[F[_]](private val dag: BlockDagRepresentation[F])
    extends AnyVal {

  /** find last finalized block, given set of latest messages. Use maxDepth put a constraint on search scope. */
  def findLastFinalizedBlock(
      latestMessagesView: Map[Validator, BlockHash],
      faultToleranceThreshold: Float,
      lowestHeight: Long = 0
  )(implicit syncF: Sync[F]): F[Option[BlockHash]] =
    latestMessagesView.toStream
      .traverse { case (v, h) => dag.lookupUnsafe(h).map((v, _)) }
      .map(_.toMap)
      .flatMap(
        Finalizer
          .findLastFinalizedBlock(dag, _, faultToleranceThreshold, lowestHeight)
      )
}
