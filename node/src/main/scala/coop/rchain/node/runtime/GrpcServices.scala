package coop.rchain.node.runtime

import cats.effect.Concurrent
import coop.rchain.casper.api.{BlockApi, BlockReportApi}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.monix.Monixable
import coop.rchain.node.api.{DeployGrpcServiceV1, ProposeGrpcServiceV1, ReplGrpcService}
import coop.rchain.node.model.repl.ReplGrpcMonix
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.shared.Log
import monix.execution.Scheduler

final case class GrpcServices(
    deploy: DeployServiceV1GrpcMonix.DeployService,
    propose: ProposeServiceV1GrpcMonix.ProposeService,
    repl: ReplGrpcMonix.Repl
)

object GrpcServices {
  def build[F[_]: Monixable: Concurrent: Log](
      blockApi: BlockApi[F],
      blockReportAPI: BlockReportApi[F],
      runtime: RhoRuntime[F]
  )(implicit mainScheduler: Scheduler): GrpcServices = {
    val repl    = ReplGrpcService(runtime, mainScheduler)
    val deploy  = DeployGrpcServiceV1(blockApi, blockReportAPI)
    val propose = ProposeGrpcServiceV1(blockApi)

    GrpcServices(deploy, propose, repl)
  }
}
