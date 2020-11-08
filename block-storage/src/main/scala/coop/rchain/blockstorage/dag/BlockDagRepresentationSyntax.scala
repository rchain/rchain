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

  /**
    * Get block metadata, "unsafe" because method expects block already in the DAG.
    *
    * Unfortunately there is no way to get stack trace when error is thrown in async execution.
    * Monix does not have support and cats.effect support is in creation.
    * https://github.com/typelevel/cats-effect/pull/854
    * So extra source parameters are a desperate measure to indicate who is the caller.
    */
  def lookupUnsafe(hash: BlockHash)(
      implicit line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing
  ): F[BlockMetadata] = {
    def source = s"${file.value}:${line.value} ${enclosing.value}"
    def errMsg = s"DAG storage is missing hash ${PrettyPrinter.buildString(hash)}\n  $source"
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

  def invalidLatestMessages: F[Map[Validator, BlockHash]] = latestMessages.flatMap(
    lm =>
      invalidLatestMessages(lm.map {
        case (validator, block) => (validator, block.blockHash)
      })
  )

  def invalidLatestMessages(
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[Map[Validator, BlockHash]] =
    dag.invalidBlocks.map { invalidBlocks =>
      latestMessagesHashes.filter {
        case (_, blockHash) => invalidBlocks.map(_.blockHash).contains(blockHash)
      }
    }

  def invalidBlocksMap: F[Map[BlockHash, Validator]] =
    for {
      ib <- dag.invalidBlocks
      r  = ib.map(block => (block.blockHash, block.sender)).toMap
    } yield r
}
