package coop.rchain.node

import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.protocol.CasperMessageGrpcMonix
import coop.rchain.catscontrib._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.grpc.{GrpcServer, Server}
import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics}
import coop.rchain.node.model.diagnostics._
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
  )(
      implicit worker: Scheduler,
      nodeDiscovery: NodeDiscovery[Task],
      jvmMetrics: JvmMetrics[Task],
      nodeMetrics: NodeMetrics[Task],
      connectionsCell: ConnectionsCell[Task]
  ): Task[Server[Task]] =
    GrpcServer[Task](
      NettyServerBuilder
        .forPort(port)
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          ReplGrpcMonix.bindService(new ReplGrpcService(runtime, worker), grpcExecutor)
        )
        .addService(DiagnosticsGrpcMonix.bindService(diagnostics.effects.grpc, grpcExecutor))
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
          CasperMessageGrpcMonix
            .bindService(DeployGrpcService.instance(blockApiLock), grpcExecutor)
        )
        .build
    )
}
