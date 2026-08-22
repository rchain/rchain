package coop.rchain.catscontrib

import cats.{Alternative, Applicative, Bifoldable, Foldable, Monad, MonoidK}

trait Alternative_[F[_]] extends MonoidK[F] {

  implicit val instance: Alternative[F]

  def unite[G[_], A](fga: F[G[A]])(implicit FM: Monad[F], G: Foldable[G]): F[A] =
    instance.unite(fga)

  def separate[G[_, _], A, B](
      fgab: F[G[A, B]]
  )(implicit FM: Monad[F], G: Bifoldable[G]): (F[A], F[B]) =
    instance.separate(fgab)

  def guard(condition: Boolean): F[Unit] =
    instance.guard(condition)

  def compose[G[_]: Applicative]: Alternative[λ[α => F[G[α]]]] =
    instance.compose[G]

  override def empty[A]: F[A] =
    instance.empty

  override def combineK[A](x: F[A], y: F[A]): F[A] =
    instance.combineK(x, y)
}

object Alternative_ {

  def apply[F[_]](implicit ev: Alternative_[F]): Alternative_[F] = ev

  implicit def alternative[F[_]](implicit ev: Alternative[F]): Alternative_[F] =
    new Alternative_[F] {
      val instance = ev
    }

}
