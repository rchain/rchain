package coop.rchain.catscontrib
import cats.MonadError

trait MonadError_[F[_], E] extends ApplicativeError_[F, E] {

  implicit val instance: MonadError[F, E]

  def ensure[A](fa: F[A])(error: => E)(predicate: A => Boolean): F[A] =
    instance.ensure(fa)(error)(predicate)

  def ensureOr[A](fa: F[A])(error: A => E)(predicate: A => Boolean): F[A] =
    instance.ensureOr(fa)(error)(predicate)

  def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A] =
    instance.adaptError(fa)(pf)

  def rethrow[A](fa: F[Either[E, A]]): F[A] =
    instance.rethrow(fa)
}

object MonadError_ {
  def apply[F[_], E](implicit F: MonadError_[F, E]): MonadError_[F, E] = F

  implicit def fromCatsMonadError[F[_], E](implicit ev: MonadError[F, E]): MonadError_[F, E] =
    new MonadError_[F, E] {
      override implicit val instance: MonadError[F, E] = ev
    }
}
