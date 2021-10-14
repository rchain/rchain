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

  /**
    * Helper extension function to lift F[A] to EitherT.right(A).
    *
    * Because function requires only left type to be specified it can be used as a shorter expression
    * which requires all three types specified.
    *
    * Example:
    *     val fa = 42.pure[F]
    *
    *     // MyError cannot be inferred so all three types must be specified.
    *     val e1: EitherT[F, MyError, Int] = EitherT.liftF[F, MyError, Int](fa)
    *
    *     // Instead fa can be lifted by specifying only left type.
    *     val e2: EitherT[F, MyError, Int] = fa.liftEitherT[MyError]
    */
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
