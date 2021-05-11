package coop.rchain.blockstorage.dag

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.Justification
import coop.rchain.dag.Casper
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator

trait BlockDagRepresentationSyntax {
  implicit final def blockStorageSyntaxBlockDagRepresentation[F[_]: Sync](
      dag: BlockDagRepresentation[F]
  ): BlockDagRepresentationOps[F] = new BlockDagRepresentationOps[F](dag)
}

final case class BlockDagInconsistencyError(message: String) extends Exception(message)
final case class NoLatestMessage(message: String)            extends Exception(message)

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

  def lookupUnsafe(hashes: Seq[BlockHash])(
      implicit line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing,
      concurrent: Concurrent[F]
  ): F[List[BlockMetadata]] = {
    val streams = hashes.map(h => fs2.Stream.eval(lookupUnsafe(h)))
    fs2.Stream.emits(streams).parJoinUnbounded.compile.toList
  }

  def latestMessageHashUnsafe(v: Validator)(
      implicit line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing
  ): F[BlockHash] = {
    def source = s"${file.value}:${line.value} ${enclosing.value}"
    def errMsg = s"No latest message for validator ${PrettyPrinter.buildString(v)}\n  $source"
    dag.latestMessageHash(v) >>= (_.liftTo(NoLatestMessage(errMsg)))
  }

  def childrensMetas(hashes: Seq[BlockHash])(
      implicit line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing,
      concurrent: Concurrent[F]
  ): F[List[BlockMetadata]] = {

    val childStream = fs2.Stream.emits(hashes).flatMap { h =>
      fs2.Stream
        .eval(dag.children(h).map(_.getOrElse(Set.empty)))
        .flatMap(xs => fs2.Stream.fromIterator(xs.iterator))
        .parEvalMapUnordered(100)(lookupUnsafe)
    }
    childStream.compile.toList

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

  def getCasperJustificationsUnsafe(
      blockHash: BlockHash
  ): F[Set[Casper.Justification[BlockHash, Validator]]] =
    lookupUnsafe(blockHash).map(
      _.justifications.map {
        case Justification(validator, latestBlockHash) =>
          Casper.Justification(latestBlockHash, validator)
      }.toSet
    )

  def toCasperJustificationUnsafe(
      blockHash: BlockHash
  ): F[Casper.Justification[BlockHash, Validator]] =
    lookupUnsafe(blockHash).map(m => Casper.Justification(m.blockHash, m.sender))
}
