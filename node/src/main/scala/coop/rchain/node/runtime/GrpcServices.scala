package coop.rchain.node.runtime

import cats.effect.Concurrent
import coop.rchain.casper.api.{BlockApi, BlockReportApi}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceFs2Grpc
import coop.rchain.casper.protocol.propose.v1.ProposeServiceFs2Grpc
import coop.rchain.monix.Monixable
import coop.rchain.node.api.{DeployGrpcServiceV1, ProposeGrpcServiceV1, ReplGrpcService}
import coop.rchain.node.model.ReplFs2Grpc
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.shared.Log
import io.grpc.Metadata

final case class GrpcServices[F[_]](
    deployService: DeployServiceFs2Grpc[F, Metadata],
    proposeService: ProposeServiceFs2Grpc[F, Metadata],
    replService: ReplFs2Grpc[F, Metadata]
)

object GrpcServices {
  def build[F[_]: Monixable: Concurrent: Log](
      blockApi: BlockApi[F],
      blockReportAPI: BlockReportApi[F],
      runtime: RhoRuntime[F]
  ): GrpcServices[F] = {
    val repl    = ReplGrpcService(runtime)
    val deploy  = DeployGrpcServiceV1(blockApi, blockReportAPI)
    val propose = ProposeGrpcServiceV1(blockApi)

    GrpcServices(deploy, propose, repl)
  }
}
