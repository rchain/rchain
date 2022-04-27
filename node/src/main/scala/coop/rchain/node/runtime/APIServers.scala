package coop.rchain.node.runtime

import cats.effect.Concurrent
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.casper.api.{BlockApi, BlockReportApi}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
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
  def build[F[_]: Monixable: Concurrent: BlockStore: RPConfAsk: ConnectionsCell: NodeDiscovery: Log](
      blockApi: BlockApi[F],
      blockReportAPI: BlockReportApi[F],
      runtime: RhoRuntime[F]
  )(implicit mainScheduler: Scheduler): APIServers = {
    val repl    = ReplGrpcService(runtime, mainScheduler)
    val deploy  = DeployGrpcServiceV1(blockApi, blockReportAPI)
    val propose = ProposeGrpcServiceV1(blockApi)
    APIServers(repl, propose, deploy)
  }
}
