package coop.rchain.casper

import cats.effect.Sync
import coop.rchain.casper.protocol.FinalizedFringe
import coop.rchain.shared.MaybeCell

object LastApprovedBlock {
  type LastApprovedBlock[F[_]] = MaybeCell[F, FinalizedFringe]

  def apply[F[_]](implicit ev: LastApprovedBlock[F]): LastApprovedBlock[F] = ev

  def of[F[_]: Sync]: F[LastApprovedBlock[F]] = MaybeCell.of[F, FinalizedFringe]

  def unsafe[F[_]: Sync](init: Option[FinalizedFringe] = None): LastApprovedBlock[F] =
    MaybeCell.unsafe[F, FinalizedFringe](init)

}
