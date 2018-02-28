package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

trait Traverse_[F[_]] { self =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit ev: Functor[F]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]])(implicit ev: Functor[F]): G[F[A]] =
    traverse(fga)(ga => ga)
}

object Traverse_ extends Traverse_Instances {
  def apply[F[_]](implicit F: Traverse_[F]): Traverse_[F] = F
}

trait Traverse_Instances {
  implicit def traverse[F[_]](implicit TR: Traverse[F]): Traverse_[F] = new Traverse_[F] {

    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(
        implicit ev: Functor[F]): G[F[B]] =
      TR.traverse(fa)(f)

  }
}
