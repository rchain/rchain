package coop.rchain.blockstorage

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.BlockMessage
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
  // Get block, "unsafe" because method expects block already in the block store.
  def getUnsafe(hash: BlockHash): F[BlockMessage] = {
    def errMsg = s"BlockStore is missing hash ${PrettyPrinter.buildString(hash)}"
    blockStore.get(hash) >>= (_.liftTo(BlockStoreInconsistencyError(errMsg)))
  }
}
