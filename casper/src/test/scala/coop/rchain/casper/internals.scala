package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import scalaz.MonadState

object internals {
  type ChainState[F[_]] = MonadState[F, Chain]
  def chainState[F[_]: ChainState]: ChainState[F] = MonadState[F, Chain]
  final case class Chain(idToBlocks: collection.Map[Int, BlockMessage],
                         hashToBlocks: collection.Map[ByteString, BlockMessage],
                         currentId: Int)
}
