package coop.rchain.catscontrib
import cats.{FlatMap, Monad}

final class FlatMapBooleanOps[F[_]: FlatMap](self: F[Boolean]) {

  def <&&>(other: => F[Boolean]): F[Boolean] = FlatMap[F].ifM(self)(other, self)
  def <||>(other: => F[Boolean]): F[Boolean] = FlatMap[F].ifM(self)(self, other)

}

trait ToFlatMapBooleanOps {
  implicit def ToFlatMapBooleanOpsFromBoolean[F[_]: Monad](
      self: F[Boolean]
  ): FlatMapBooleanOps[F] =
    new FlatMapBooleanOps(self)
}
