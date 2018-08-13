package coop.rchain.node.api

import cats._
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.protocol.DeployServiceGrpc
import coop.rchain.casper.{MultiParentCasperRef, SafetyOracle}
import coop.rchain.catscontrib._
import coop.rchain.comm.discovery._
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
      F[_]: Capture: Functor: NodeDiscovery: JvmMetrics: NodeMetrics: Futurable](
      port: Int,
      runtime: Runtime)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port)
        .addService(ReplGrpc.bindService(new ReplGrpcService(runtime), scheduler))
        .addService(DiagnosticsGrpc.bindService(diagnostics.grpc[F], scheduler))
        .build
    }

  def acquireExternalServer[
      F[_]: Capture: Monad: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Futurable](
      port: Int)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port)
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
