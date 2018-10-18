package coop.rchain.casper

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.shared.{Log, LogSource, MaybeCell}

object MultiParentCasperRef {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  type MultiParentCasperRef[F[_]] = MaybeCell[F, MultiParentCasper[F]]

  def apply[F[_]](implicit ev: MultiParentCasperRef[F]): MultiParentCasperRef[F] = ev

  private class MultiParentCasperRefImpl[F[_]](state: Ref[F, Option[MultiParentCasper[F]]])
      extends MultiParentCasperRef[F] {
    override def get: F[Option[MultiParentCasper[F]]]       = state.get
    override def set(casper: MultiParentCasper[F]): F[Unit] = state.set(Some(casper))
  }

  def of[F[_]: Sync]: F[MultiParentCasperRef[F]] = MaybeCell.of[F, MultiParentCasper[F]]

  // For usage in tests only
  def unsafe[F[_]: Sync](casperRef: Option[MultiParentCasper[F]] = None): MultiParentCasperRef[F] =
    MaybeCell.unsafe[F, MultiParentCasper[F]](casperRef)

  def withCasper[F[_]: Monad: Log: MultiParentCasperRef, A](
      f: MultiParentCasper[F] => F[A],
      default: A
  ): F[A] =
    MultiParentCasperRef[F].get flatMap {
      case Some(casper) => f(casper)
      case None =>
        Log[F].warn(s"Casper instance was not available.").map(_ => default)
    }
}
