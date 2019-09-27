package coop.rchain.casper

import coop.rchain.casper.engine._, EngineCell._
import cats._, cats.data._, cats.implicits._
import coop.rchain.casper.protocol.CasperMessage
import coop.rchain.comm.PeerNode

class EngineWithCasper[F[_]: Applicative](casper: MultiParentCasper[F]) extends Engine[F] {
  override val init                                                = Applicative[F].unit
  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = Applicative[F].unit
  override def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] = f(casper)
}
