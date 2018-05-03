package coop.rchain.casper

import cats._
import cats.mtl.MonadState

object BlockDagState {
  type BlockDagState[F[_]] = MonadState[F, BlockDag]
  def blockDagState[F[_]: Monad: BlockDagState]: BlockDagState[F] = MonadState[F, BlockDag]
}
