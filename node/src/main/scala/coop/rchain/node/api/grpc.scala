package coop.rchain.node.api

import coop.rchain.node.diagnostics
import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}
import io.grpc.netty.NettyServerBuilder

import scala.concurrent.Future
import cats._
import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.protocol.DeployServiceGrpc
import coop.rchain.catscontrib._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.node.diagnostics
import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics}
import coop.rchain.node.model.diagnostics._
import coop.rchain.node.model.repl._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared._
import io.grpc.{Server, ServerBuilder}
import monix.execution.Scheduler

object GrpcServer {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def acquireInternalServer[
      F[_]: Capture: Functor: NodeDiscovery: JvmMetrics: NodeMetrics: ConnectionsCell: Futurable](
      port: Int,
      maxMessageSize: Int,
      runtime: Runtime)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      NettyServerBuilder
        .forPort(port)
        .maxMessageSize(maxMessageSize)
        .addService(ReplGrpc.bindService(new ReplGrpcService(runtime), scheduler))
        .addService(DiagnosticsGrpc.bindService(diagnostics.grpc[F], scheduler))
        .build
    }

  def acquireExternalServer[
      F[_]: Sync: Capture: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Futurable](
      port: Int,
      maxMessageSize: Int)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      NettyServerBuilder
        .forPort(port)
        .maxMessageSize(maxMessageSize)
        .addService(DeployServiceGrpc.bindService(new DeployGrpcService[F], scheduler))
        .build
    }

  def start[F[_]: FlatMap: Capture: Log](serverExternal: Server, serverInternal: Server): F[Unit] =
    for {
      _ <- Capture[F].capture(serverExternal.start)
      _ <- Capture[F].capture(serverInternal.start)
      _ <- Log[F].info("gRPC server started, listening on ")
    } yield ()

}
