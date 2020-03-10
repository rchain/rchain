package coop.rchain.catscontrib

import cats._, cats.syntax.all._

final class BooleanF[F[_]: FlatMap](val self: F[Boolean]) {
  def &&^(other: => F[Boolean]): F[Boolean] =
    BooleanF.&&^[F](self, other)

  def ||^(other: => F[Boolean]): F[Boolean] =
    BooleanF.||^[F](self, other)

  def not: F[Boolean] = BooleanF.~^(self)
}

object BooleanF {
  def &&^[F[_]: FlatMap](m1: F[Boolean], m2: => F[Boolean]): F[Boolean] =
    FlatMap[F].ifM(m1)(m2, m1)

  def ||^[F[_]: FlatMap](m1: F[Boolean], m2: => F[Boolean]): F[Boolean] =
    FlatMap[F].ifM(m1)(m1, m2)

  def ~^[F[_]: FlatMap](m: F[Boolean]): F[Boolean] =
    m.map(!_)
}

trait ToBooleanF {
  implicit def ToBooleanF[F[_]: FlatMap](v: F[Boolean]): BooleanF[F] =
    new BooleanF[F](v)
}
