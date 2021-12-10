package coop.rchain.node.api

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.casper.{
  LastFinalizedHeightConstraintChecker,
  ProposeFunction,
  SynchronyConstraintChecker
}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log

trait AdminWebApi[F[_]] {
  def propose: F[String]
  def proposeResult: F[String]
}

object AdminWebApi {
  class AdminWebApiImpl[F[_]: Concurrent: EngineCell: SynchronyConstraintChecker: Log: Span: Metrics](
      triggerProposeFOpt: Option[ProposeFunction[F]],
      proposerStateRefOpt: Option[Ref[F, ProposerState[F]]]
  ) extends AdminWebApi[F] {
    import WebApiSyntax._

    def propose: F[String] =
      triggerProposeFOpt match {
        case Some(q) =>
          BlockAPI.createBlock[F](q).flatMap(_.liftToBlockApiErr)
        case None => "Propose error: read-only node.".asLeft[String].liftToBlockApiErr
      }

    def proposeResult: F[String] =
      proposerStateRefOpt match {
        case Some(s) =>
          BlockAPI.getProposeResult[F](s).flatMap(_.liftToBlockApiErr)
        case None => "Error: read-only node.".asLeft[String].liftToBlockApiErr
      }

  }
}
