package coop.rchain.node

import cats.effect.Concurrent

import coop.rchain.casper.protocol.{DeployServiceGrpcMonix, ProposeServiceGrpcMonix}
import coop.rchain.casper.protocol.deployV2.DeployServiceV2GrpcMonix
import coop.rchain.catscontrib._
import coop.rchain.grpc.{GrpcServer, Server}
import coop.rchain.node.model.repl._
import coop.rchain.shared._

import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import monix.eval.Task
import monix.execution.Scheduler

package object api {

  // 16 MB is max message size allowed by HTTP2 RFC 7540
  // grpc and netty can however work with bigger values
  val maxMessageSize: Int = 16 * 1024 * 1024

  def acquireInternalServer(
      port: Int,
      grpcExecutor: Scheduler,
      replGrpcService: ReplGrpcMonix.Repl,
      proposeGrpcService: ProposeServiceGrpcMonix.ProposeService
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
          ProposeServiceGrpcMonix
            .bindService(proposeGrpcService, grpcExecutor)
        )
        .addService(ProtoReflectionService.newInstance())
        .build
    )

  def acquireExternalServer[F[_]: Concurrent: Log: Taskable](
      port: Int,
      grpcExecutor: Scheduler,
      deployGrpcService: DeployServiceGrpcMonix.DeployService,
      deployGrpcServiceV2: DeployServiceV2GrpcMonix.DeployServiceV2,
      proposeGrpcService: ProposeServiceGrpcMonix.ProposeService
  ): F[Server[F]] =
    GrpcServer[F](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          DeployServiceGrpcMonix
            .bindService(deployGrpcService, grpcExecutor)
        )
        .addService(
          DeployServiceV2GrpcMonix
            .bindService(deployGrpcServiceV2, grpcExecutor)
        )
        .addService(
          ProposeServiceGrpcMonix
            .bindService(proposeGrpcService, grpcExecutor)
        )
        .addService(ProtoReflectionService.newInstance())
        .build
    )
}
