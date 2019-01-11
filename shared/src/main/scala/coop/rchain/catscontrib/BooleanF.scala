package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

final class BooleanF[F[_]: Applicative](val self: F[Boolean]) {
  def &&^(other: F[Boolean]): F[Boolean] =
    BooleanF.&&^[F](self, other)
  def ||^(other: F[Boolean]): F[Boolean] =
    BooleanF.&&^[F](self, other)
  def ~^(): F[Boolean] = BooleanF.~^[F](self)
}

object BooleanF {
  def &&^[F[_]: Applicative](m1: F[Boolean], m2: F[Boolean]): F[Boolean] =
    Applicative[F].map2(m1, m2)(_ && _)

  def ||^[F[_]: Applicative](m1: F[Boolean], m2: F[Boolean]): F[Boolean] =
    Applicative[F].map2(m1, m2)(_ && _)

  def ~^[F[_]: Applicative](m: F[Boolean]): F[Boolean] =
    m.map(!_)
}

trait ToBooleanF {
  implicit def ToBooleanF[F[_]: Applicative](v: F[Boolean]): BooleanF[F] =
    new BooleanF[F](v)
}
