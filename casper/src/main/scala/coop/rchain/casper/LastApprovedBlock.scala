package coop.rchain.casper

import cats.Monad
import cats.data.EitherT
import cats.effect.Sync
import cats.effect.concurrent.{Deferred, Ref}
import cats.implicits._
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans

trait LastApprovedBlock[F[_]] {
  def get: F[Option[ApprovedBlock]]
  def set(a: ApprovedBlock): F[Unit]
}

object LastApprovedBlock extends LastApprovedBlockInstances {
  def apply[F[_]](implicit ev: LastApprovedBlock[F]): LastApprovedBlock[F] = ev

  def of[F[_]: Sync]: F[LastApprovedBlock[F]] =
    Ref.of[F, Option[ApprovedBlock]](None).map { state =>
      new LastApprovedBlock[F] {
        override def get: F[Option[ApprovedBlock]] = state.get
        override def set(a: ApprovedBlock): F[Unit] =
          state.set(Some(a))
      }
    }

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: LastApprovedBlock[F]): LastApprovedBlock[T[F, ?]] =
    new LastApprovedBlock[T[F, ?]] {
      override def get: T[F, Option[ApprovedBlock]] =
        C.get.liftM[T]
      override def set(a: ApprovedBlock): T[F, Unit] =
        C.set(a).liftM[T]
    }

}

sealed abstract class LastApprovedBlockInstances {
  implicit def eitherTLastApprovedBlock[E, F[_]: Monad: LastApprovedBlock[?[_]]]
    : LastApprovedBlock[EitherT[F, E, ?]] =
    LastApprovedBlock.forTrans[F, EitherT[?[_], E, ?]]
}
