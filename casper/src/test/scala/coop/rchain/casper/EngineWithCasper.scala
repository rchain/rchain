package coop.rchain.casper

import coop.rchain.casper.engine._, EngineCell._
import cats._, cats.data._, cats.implicits._

class EngineWithCasper[F[_]: Applicative](casper: MultiParentCasper[F]) extends Engine[F] {
  override def applicative: Applicative[F] = Applicative[F]
  override def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] = f(casper)
}
