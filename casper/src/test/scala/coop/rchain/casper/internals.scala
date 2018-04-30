package coop.rchain.casper

import cats._
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol.BlockMessage

import scala.collection.immutable.{HashMap, HashSet}

object internals {
  type ChainState[F[_]] = MonadState[F, Chain]
  def chainState[F[_]: Monad: ChainState]: ChainState[F] = MonadState[F, Chain]

  // TODO: Move to src/ and merge with existing mutable data structures in Casper.scala
  final case class Chain(idToBlocks: HashMap[Int, BlockMessage],
                         blockLookup: HashMap[ByteString, BlockMessage],
                         childMap: HashMap[BlockHash, HashSet[BlockHash]],
                         currentId: Int)
}
