package coop.rchain.shared
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._

trait MaybeCell[F[_], A] {
  def get: F[Option[A]]
  def set(a: A): F[Unit]
}

object MaybeCell {
  def apply[F[_], A](implicit ev: MaybeCell[F, A]): MaybeCell[F, A] = ev

  def of[F[_]: Sync, A]: F[MaybeCell[F, A]] =
    Ref.of[F, Option[A]](None).map { state =>
      new MaybeCell[F, A] {
        override def get: F[Option[A]] = state.get
        override def set(a: A): F[Unit] =
          state.set(Some(a))
      }
    }

  def unsafe[F[_]: Sync, A](init: Option[A]): MaybeCell[F, A] =
    new MaybeCell[F, A] {
      private val state: Ref[F, Option[A]] = Ref.unsafe[F, Option[A]](init)
      override def get: F[Option[A]]       = state.get
      override def set(a: A): F[Unit]      = state.set(Some(a))
    }

}
