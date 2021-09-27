package coop.rchain.node.runtime

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.casper.state.instances.ProposerState
import coop.rchain.casper._
import coop.rchain.casper.api.BlockReportAPI
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.monix.Monixable
import coop.rchain.node.api.{DeployGrpcServiceV1, ProposeGrpcServiceV1, ReplGrpcService}
import coop.rchain.node.model.repl.ReplGrpcMonix
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.shared.Log
import monix.execution.Scheduler

final case class APIServers(
    repl: ReplGrpcMonix.Repl,
    propose: ProposeServiceV1GrpcMonix.ProposeService,
    deploy: DeployServiceV1GrpcMonix.DeployService
)

object APIServers {
  def build[F[_]: Monixable](
      runtime: RhoRuntime[F],
      triggerProposeFOpt: Option[ProposeFunction[F]],
      proposerStateRefOpt: Option[Ref[F, ProposerState[F]]],
      apiMaxBlocksLimit: Int,
      devMode: Boolean,
      proposeFOpt: Option[ProposeFunction[F]],
      blockReportAPI: BlockReportAPI[F]
  )(
      implicit
      blockStore: BlockStore[F],
      concurrent: Concurrent[F],
      metrics: Metrics[F],
      span: Span[F],
      engineCell: EngineCell[F],
      logF: Log[F],
      mainScheduler: Scheduler
  ): APIServers = {
    val repl = ReplGrpcService(runtime, mainScheduler)
    val deploy =
      DeployGrpcServiceV1(
        apiMaxBlocksLimit,
        blockReportAPI,
        proposeFOpt,
        devMode
      )
    val propose = ProposeGrpcServiceV1(triggerProposeFOpt, proposerStateRefOpt)
    APIServers(repl, propose, deploy)
  }
}
