package coop.rchain.node

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski.kp

object DiagnosticsRuntime {
  def diagnosticsProgram[F[_]: Monad: ConsoleIO: DiagnosticsService]: F[Unit] =
    for {
      _  <- ConsoleIO[F].println("List of peers:")
      ps <- DiagnosticsService[F].listPeers
      _  <- ps.traverse(p => ConsoleIO[F].println(p.toAddress))
    } yield ()
}
