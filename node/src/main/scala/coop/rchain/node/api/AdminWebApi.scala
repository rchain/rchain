package coop.rchain.node.api

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.ProposeFunction
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.state.RNodeStateManager
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log
import coop.rchain.models.syntax._

trait AdminWebApi[F[_]] {
  def propose: F[String]
  def proposeResult: F[String]
  def cbuf: F[String]
}

object AdminWebApi {
  class AdminWebApiImpl[F[_]: Concurrent: EngineCell: Log: Span: Metrics: RNodeStateManager](
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

    override def cbuf: F[String] =
      RNodeStateManager[F].casperBufferStorage.toMap.map { c =>
        c.map { case (h, s) => s"${h.show} -> $s" }.mkString("\n")
      }
  }
}
