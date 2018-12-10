package coop.rchain.node.api

import cats.effect.Sync

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.protocol.CasperMessageGrpcMonix
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
import java.util.concurrent.TimeUnit
import monix.eval.Task
import monix.execution.Scheduler
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, ski._

class GrpcServer(server: Server) {
  def start: Task[Unit] = Task.delay(server.start())

  private def attemptShutdown: Task[Boolean] =
    (for {
      _          <- Task.delay(server.shutdown())
      _          <- Task.delay(server.awaitTermination(1000, TimeUnit.MILLISECONDS))
      terminated <- Task.delay(server.isTerminated)
    } yield terminated).attempt map (_.fold(kp(false), id))

  private def shutdownImmediately: Task[Unit] =
    Task.delay(server.shutdownNow()).attempt.as(())

  def stop: Task[Unit] = attemptShutdown >>= { stopped =>
    if (stopped) Task.unit else shutdownImmediately
  }
  def port: Int = server.getPort
}

object GrpcServer {

  def apply(server: Server): GrpcServer = new GrpcServer(server)

  def acquireInternalServer(
      port: Int,
      maxMessageSize: Int,
      runtime: Runtime,
      grpcExecutor: Scheduler
  )(
      implicit worker: Scheduler,
      nodeDiscovery: NodeDiscovery[Task],
      jvmMetrics: JvmMetrics[Task],
      nodeMetrics: NodeMetrics[Task],
      connectionsCell: ConnectionsCell[Task]
  ): Task[GrpcServer] =
    Task.delay {
      GrpcServer(
        NettyServerBuilder
          .forPort(port)
          .executor(grpcExecutor)
          .maxMessageSize(maxMessageSize)
          .addService(
            ReplGrpcMonix.bindService(new ReplGrpcService(runtime, worker), grpcExecutor)
          )
          .addService(DiagnosticsGrpcMonix.bindService(diagnostics.grpc, grpcExecutor))
          .build
      )
    }

  def acquireExternalServer[F[_]: Sync: Capture: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable](
      port: Int,
      maxMessageSize: Int,
      grpcExecutor: Scheduler
  )(implicit worker: Scheduler): F[GrpcServer] =
    Capture[F].capture {
      GrpcServer(
        NettyServerBuilder
          .forPort(port)
          .executor(grpcExecutor)
          .maxMessageSize(maxMessageSize)
          .addService(
            CasperMessageGrpcMonix.bindService(DeployGrpcService.instance, grpcExecutor)
          )
          .build
      )
    }
}
