package coop.rchain.casper

import cats._
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage

object internals {
  type ChainState[F[_]] = MonadState[F, Chain]
  def chainState[F[_]: Monad: ChainState]: ChainState[F] = MonadState[F, Chain]
  final case class Chain(idToBlocks: collection.Map[Int, BlockMessage],
                         hashToBlocks: collection.Map[ByteString, BlockMessage],
                         currentId: Int)
}
