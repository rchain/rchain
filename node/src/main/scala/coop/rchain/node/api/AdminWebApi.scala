package coop.rchain.node.api

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.blocks.proposer.Proposer
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.casper.{Casper, LastFinalizedHeightConstraintChecker, SynchronyConstraintChecker}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log
import coop.rchain.state.StateManager
import fs2.concurrent.Queue

trait AdminWebApi[F[_]] {
  def propose: F[String]
  def proposeResult: F[String]
}

object AdminWebApi {
  class AdminWebApiImpl[F[_]: Concurrent: EngineCell: SynchronyConstraintChecker: LastFinalizedHeightConstraintChecker: Log: Span: Metrics](
      proposerQueueOpt: Option[Queue[F, (Casper[F], Deferred[F, Option[Int]])]],
      proposerStateRef: Option[Ref[F, ProposerState[F]]],
      stateManager: StateManager[F]
  ) extends AdminWebApi[F] {
    import WebApiSyntax._

    def propose: F[String] =
      proposerQueueOpt match {
        case Some(q) =>
          BlockAPI.createBlock[F](q).flatMap(_.liftToBlockApiErr)
        case None => "Propose error: read-only node.".asLeft[String].liftToBlockApiErr
      }

    def proposeResult: F[String] =
      proposerStateRef match {
        case Some(s) =>
          BlockAPI.getProposeResult[F](s).flatMap(_.liftToBlockApiErr)
        case None => "Error: read-only node.".asLeft[String].liftToBlockApiErr
      }

  }
}
