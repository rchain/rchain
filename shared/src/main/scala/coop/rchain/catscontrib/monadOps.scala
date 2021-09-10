package coop.rchain.catscontrib

import cats.{Functor, Monad}
import cats.data.EitherT
import cats.syntax.all._
import coop.rchain.catscontrib.ski.kp

// TODO: Rename this syntax to include functor, monad, ... (HKTSyntax?)

final class MonadOps[F[_], A](val self: F[A]) extends AnyVal {
  def liftM[G[_[_], _]](implicit M: Monad[F], G: MonadTrans[G]): G[F, A] = G.liftM(self)
  def forever(implicit M: Monad[F]): F[A]                                = MonadOps.forever(self)

  /* EitherT syntax */

  def liftEitherT[L](implicit f: Functor[F]): EitherT[F, L, A] = EitherT.liftF[F, L, A](self)
}

object MonadOps {
  def forever[F[_]: Monad, A](fa: F[A]): F[A] =
    fa >>= kp(forever(fa))
  def forever[F[_]: Monad, A](fa: A => F[A], a: A): F[A] =
    fa(a) >>= (na => forever(fa, na))
}

trait ToMonadOps {
  implicit def ToMonadOps[F[_], A](v: F[A]): MonadOps[F, A] = new MonadOps[F, A](v)
}
