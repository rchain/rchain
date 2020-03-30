package coop.rchain.node

import cats.effect.Concurrent

import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.catscontrib._
import coop.rchain.grpc.{GrpcServer, Server}
import coop.rchain.node.model.repl._
import coop.rchain.shared._

import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import monix.eval.Task
import monix.execution.Scheduler

package object api {

  def acquireInternalServer(
      port: Int,
      grpcExecutor: Scheduler,
      replGrpcService: ReplGrpcMonix.Repl,
      deployGrpcService: DeployServiceV1GrpcMonix.DeployService,
      proposeGrpcService: ProposeServiceV1GrpcMonix.ProposeService,
      maxMessageSize: Int
  ): Task[Server[Task]] =
    GrpcServer[Task](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          ReplGrpcMonix.bindService(replGrpcService, grpcExecutor)
        )
        .addService(
          ProposeServiceV1GrpcMonix
            .bindService(proposeGrpcService, grpcExecutor)
        )
        .addService(
          DeployServiceV1GrpcMonix
            .bindService(deployGrpcService, grpcExecutor)
        )
        .addService(ProtoReflectionService.newInstance())
        .build
    )

  def acquireExternalServer[F[_]: Concurrent: Log: Taskable](
      port: Int,
      grpcExecutor: Scheduler,
      deployGrpcService: DeployServiceV1GrpcMonix.DeployService,
      maxMessageSize: Int
  ): F[Server[F]] =
    GrpcServer[F](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          DeployServiceV1GrpcMonix
            .bindService(deployGrpcService, grpcExecutor)
        )
        .addService(ProtoReflectionService.newInstance())
        .build
    )
}
