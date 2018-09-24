package coop.rchain.casper

import cats.Monad
import cats.data.EitherT
import cats.effect.Sync
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.shared.MaybeCell

object LastApprovedBlock extends LastApprovedBlockInstances {
  type LastApprovedBlock[F[_]] = MaybeCell[F, ApprovedBlock]

  def apply[F[_]](implicit ev: LastApprovedBlock[F]): LastApprovedBlock[F] = ev

  def of[F[_]: Sync]: F[LastApprovedBlock[F]] = MaybeCell.of[F, ApprovedBlock]

  def unsafe[F[_]: Sync](init: Option[ApprovedBlock] = None): LastApprovedBlock[F] =
    MaybeCell.unsafe[F, ApprovedBlock](init)

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: LastApprovedBlock[F]
  ): LastApprovedBlock[T[F, ?]] =
    new MaybeCell[T[F, ?], ApprovedBlock] {
      override def get: T[F, Option[ApprovedBlock]] =
        C.get.liftM[T]
      override def set(a: ApprovedBlock): T[F, Unit] =
        C.set(a).liftM[T]
    }

}

sealed abstract class LastApprovedBlockInstances {
  implicit def eitherTLastApprovedBlock[E, F[_]: Monad: MaybeCell[?[_], ApprovedBlock]]
    : MaybeCell[EitherT[F, E, ?], ApprovedBlock] =
    LastApprovedBlock.forTrans[F, EitherT[?[_], E, ?]]
}
