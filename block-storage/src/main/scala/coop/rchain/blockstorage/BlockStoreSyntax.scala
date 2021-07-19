package coop.rchain.blockstorage

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{BlockMessage, ProcessedDeploy}
import coop.rchain.models.BlockHash.BlockHash

trait BlockStoreSyntax {
  implicit final def syntaxBlockStore[F[_]: Sync](
      blockStore: BlockStore[F]
  ): BlockStoreOps[F] =
    new BlockStoreOps[F](blockStore)
}

/**
  * This kind of error should be in a group of fatal errors.
  * E.g. for unsafe `get` we are expecting the message to be in the store and if it's not, node should stop.
  *
  * The point is to have the syntax to recognize these places, categorize errors and have meaningful error messages
  * instead of generic text e.g. for Option - NoSuchElementException: None.get.
  */
final case class BlockStoreInconsistencyError(message: String) extends Exception(message)

final class BlockStoreOps[F[_]: Sync](
    // BlockStore extensions / syntax
    private val blockStore: BlockStore[F]
) {

  /**
    * Get block, "unsafe" because method expects block already in the block store.
    *
    * Unfortunately there is no way to get stack trace when error is thrown in async execution.
    * Monix does not have support and cats.effect support is in creation.
    * https://github.com/typelevel/cats-effect/pull/854
    * So extra source parameters are a desperate measure to indicate who is the caller.
    */
  def getUnsafe(hash: BlockHash)(
      implicit line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing
  ): F[BlockMessage] = {
    def source = s"${file.value}:${line.value} ${enclosing.value}"
    def errMsg = s"BlockStore is missing hash ${PrettyPrinter.buildStringNoLimit(hash)}\n  $source"
    blockStore.get(hash) >>= (_.liftTo(BlockStoreInconsistencyError(errMsg)))
  }

  def getUnsafe(
      hashes: Seq[BlockHash]
  )(implicit concurrent: Concurrent[F]): fs2.Stream[F, BlockMessage] = {
    val streams = hashes.map(h => fs2.Stream.eval(getUnsafe(h)))
    fs2.Stream
      .emits(streams)
      .parJoinUnbounded
  }
}
