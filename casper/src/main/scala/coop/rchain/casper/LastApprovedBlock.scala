package coop.rchain.casper

import cats.effect.Sync
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.shared.MaybeCell

object LastApprovedBlock {
  type LastApprovedBlock[F[_]] = MaybeCell[F, ApprovedBlock]

  def apply[F[_]](implicit ev: LastApprovedBlock[F]): LastApprovedBlock[F] = ev

  def of[F[_]: Sync]: F[LastApprovedBlock[F]] = MaybeCell.of[F, ApprovedBlock]

  def unsafe[F[_]: Sync](init: Option[ApprovedBlock] = None): LastApprovedBlock[F] =
    MaybeCell.unsafe[F, ApprovedBlock](init)

}
