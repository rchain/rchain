package coop.rchain.node.api

import coop.rchain.node.diagnostics
import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}
import io.grpc.netty.NettyServerBuilder

import scala.concurrent.Future
import cats._
import cats.data._
import cats.implicits._
import com.google.protobuf.empty.Empty
import coop.rchain.casper.{MultiParentCasperConstructor, PrettyPrinter, SafetyOracle}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc, DeployServiceResponse}
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.catscontrib._
import Catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.node.model.repl._
import coop.rchain.node.model.diagnostics._
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import monix.execution.Scheduler
import com.google.protobuf.ByteString
import java.io.{Reader, StringReader}

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.api.BlockAPI
import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics}
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._

object GrpcServer {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def acquireInternalServer[
      F[_]: Capture: Functor: NodeDiscovery: JvmMetrics: NodeMetrics: Futurable](
      port: Int,
      runtime: Runtime)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      NettyServerBuilder
        .forPort(port)
        .addService(ReplGrpc.bindService(new ReplGrpcService(runtime), scheduler))
        .addService(DiagnosticsGrpc.bindService(diagnostics.grpc[F], scheduler))
        .build
    }

  def acquireExternalServer[
      F[_]: Capture: Monad: MultiParentCasperConstructor: Log: SafetyOracle: BlockStore: Futurable](
      port: Int)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      NettyServerBuilder
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
