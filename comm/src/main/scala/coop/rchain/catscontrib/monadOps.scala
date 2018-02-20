package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

final class MonadOps[F[_], A](val self: F[A])(implicit val F: Monad[F]) {
  def liftM[G[_[_], _]](implicit G: MonadTrans[G]): G[F, A] = G.liftM(self)
}

trait ToMonadOps {
  implicit def ToMonadOps[F[_], A](v: F[A])(implicit F0: Monad[F]): MonadOps[F, A] =
    new MonadOps[F, A](v)
}
