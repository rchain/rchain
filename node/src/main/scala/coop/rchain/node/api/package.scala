package coop.rchain.node

import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.protocol.DeployServiceGrpcMonix
import coop.rchain.catscontrib._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.grpc.{GrpcServer, Server}
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
      grpcExecutor: Scheduler
  )(implicit worker: Scheduler): Task[Server[Task]] =
    GrpcServer[Task](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          ReplGrpcMonix.bindService(new ReplGrpcService(runtime, worker), grpcExecutor)
        )
        .build
    )

  def acquireExternalServer[F[_]: Concurrent: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable](
      port: Int,
      grpcExecutor: Scheduler,
      blockApiLock: Semaphore[F]
  )(implicit worker: Scheduler): F[Server[F]] =
    GrpcServer[F](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          DeployServiceGrpcMonix
            .bindService(DeployGrpcService.instance(blockApiLock), grpcExecutor)
        )
        .build
    )
}
