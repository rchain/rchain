package coop.rchain.node.api

import cats.effect.Sync

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.protocol.CasperMessageGrpcMonix
import coop.rchain.catscontrib._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.node.diagnostics
import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics}
import coop.rchain.node.model.diagnostics._
import coop.rchain.node.model.repl._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared._

import io.grpc.netty.NettyServerBuilder
import io.grpc.Server
import monix.eval.Task
import monix.execution.Scheduler

object GrpcServer {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def acquireInternalServer(port: Int, maxMessageSize: Int, runtime: Runtime)(
      implicit scheduler: Scheduler,
      nodeDiscovery: NodeDiscovery[Task],
      jvmMetrics: JvmMetrics[Task],
      nodeMetrics: NodeMetrics[Task],
      connectionsCell: ConnectionsCell[Task]
  ): Task[Server] =
    Task.delay {
      NettyServerBuilder
        .forPort(port)
        .executor(scheduler)
        .maxMessageSize(maxMessageSize)
        .addService(ReplGrpcMonix.bindService(new ReplGrpcService(runtime), scheduler))
        .addService(DiagnosticsGrpcMonix.bindService(diagnostics.grpc, scheduler))
        .build
    }

  def acquireExternalServer[F[_]: Sync: Capture: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable](
      port: Int,
      maxMessageSize: Int
  )(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      NettyServerBuilder
        .forPort(port)
        .executor(scheduler)
        .maxMessageSize(maxMessageSize)
        .addService(CasperMessageGrpcMonix.bindService(DeployGrpcService.instance, scheduler))
        .build
    }

  def start(serverExternal: Server, serverInternal: Server)(implicit log: Log[Task]): Task[Unit] =
    for {
      _ <- Task.delay(serverExternal.start)
      _ <- Task.delay(serverInternal.start)
      _ <- log.info("gRPC server started, listening on ")
    } yield ()

}
