package coop.rchain.node

import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore
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
      blockApiLock: Semaphore[Task],
      tracing: Boolean
  )(
      implicit worker: Scheduler,
      safetyOracle: SafetyOracle[Task],
      blocStore: BlockStore[Task],
      log: Log[Task],
      span: Span[Task],
      engineCell: EngineCell[Task]
  ): Task[Server[Task]] =
    GrpcServer[Task](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          ReplGrpcMonix.bindService(new ReplGrpcService(runtime, worker), grpcExecutor)
        )
        .addService(
          ProposeServiceGrpcMonix
            .bindService(ProposeGrpcService.instance(blockApiLock, tracing), grpcExecutor)
        )
        .build
    )

  def acquireExternalServer[F[_]: Concurrent: Log: SafetyOracle: BlockStore: Taskable: Span](
      port: Int,
      grpcExecutor: Scheduler,
      blockApiLock: Semaphore[F],
      tracing: Boolean
  )(implicit worker: Scheduler, engineCell: EngineCell[F]): F[Server[F]] =
    GrpcServer[F](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          DeployServiceGrpcMonix
            .bindService(DeployGrpcService.instance(blockApiLock, tracing), grpcExecutor)
        )
        .addService(
          ProposeServiceGrpcMonix
            .bindService(ProposeGrpcService.instance(blockApiLock, tracing), grpcExecutor)
        )
        .build
    )
}
