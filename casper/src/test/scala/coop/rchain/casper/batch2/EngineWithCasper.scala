package coop.rchain.casper.batch2

import cats._
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.engine._
import coop.rchain.casper.protocol.CasperMessage
import coop.rchain.comm.PeerNode

class EngineWithCasper[F[_]: Applicative](casper: MultiParentCasper[F]) extends Engine[F] {
  override val init = Applicative[F].unit
  override def handle(
      peer: PeerNode,
      msg: CasperMessage,
      disableCostAccounting: Boolean = false
  ): F[Unit] = Applicative[F].unit
  override def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] = f(casper)
}
