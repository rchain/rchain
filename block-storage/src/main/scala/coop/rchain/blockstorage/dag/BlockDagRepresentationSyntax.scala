package coop.rchain.blockstorage.dag

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator

trait BlockDagRepresentationSyntax {
  implicit final def blockStorageSyntaxBlockDagRepresentation[F[_]: Sync](
      dag: BlockDagRepresentation[F]
  ): BlockDagRepresentationOps[F] = new BlockDagRepresentationOps[F](dag)
}

final case class BlockDagInconsistencyError(message: String) extends Exception(message)

final class BlockDagRepresentationOps[F[_]: Sync](
    // BlockDagRepresentation extensions / syntax
    private val dag: BlockDagRepresentation[F]
) {
  // Get block metadata, "unsafe" because method expects block already in the DAG.
  def lookupUnsafe(hash: BlockHash): F[BlockMetadata] = {
    def errMsg = s"Block hash ${PrettyPrinter.buildString(hash)} not found in the DAG."
    dag.lookup(hash) >>= (_.liftTo(BlockDagInconsistencyError(errMsg)))
  }

  def latestMessage(validator: Validator): F[Option[BlockMetadata]] = {
    import cats.instances.option._
    dag.latestMessageHash(validator) >>= (_.traverse(lookupUnsafe))
  }

  def latestMessages: F[Map[Validator, BlockMetadata]] = {
    import cats.instances.vector._
    dag.latestMessageHashes >>= (
      _.toVector
        .traverse { case (validator, hash) => lookupUnsafe(hash).map(validator -> _) }
        .map(_.toMap)
      )
  }
}
