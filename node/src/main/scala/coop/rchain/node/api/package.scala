package coop.rchain.node

import cats.effect.Concurrent
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.protocol.{DeployServiceGrpcMonix, ProposeServiceGrpcMonix}
import coop.rchain.catscontrib._
import coop.rchain.grpc.{GrpcServer, Server}
import coop.rchain.metrics.Span
import coop.rchain.node.model.repl._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared._
import io.grpc.netty.NettyServerBuilder
import monix.eval.Task
import monix.execution.Scheduler

package object api {

  // 16 MB is max message size allowed by HTTP2 RFC 7540
  // grpc and netty can however work with bigger values
  val maxMessageSize: Int = 16 * 1024 * 1024

  def acquireInternalServer(
      port: Int,
      runtime: Runtime[Task],
      grpcExecutor: Scheduler,
      replGrpcService: ReplGrpcService,
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
        .build
    )

  def acquireExternalServer[F[_]: Concurrent: Log: Taskable](
      port: Int,
      grpcExecutor: Scheduler,
      deployGrpcService: DeployServiceGrpcMonix.DeployService,
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
          ProposeServiceGrpcMonix
            .bindService(proposeGrpcService, grpcExecutor)
        )
        .build
    )
}
