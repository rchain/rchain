package coop.rchain.node.api

import coop.rchain.node.diagnostics
import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}

import scala.concurrent.Future
import cats._
import cats.data._
import cats.implicits._
import com.google.protobuf.empty.Empty
import coop.rchain.casper.{MultiParentCasperConstructor, PrettyPrinter, SafetyOracle}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc, DeployServiceResponse, DeployString}
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

import coop.rchain.casper.api.BlockAPI
import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics, StoreMetrics}
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._

object GrpcServer {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def acquireServer[
      F[_]: Capture: Monad: MultiParentCasperConstructor: Log: NodeDiscovery: StoreMetrics: JvmMetrics: NodeMetrics: Futurable: SafetyOracle](
      port: Int,
      runtime: Runtime)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port)
        .addService(ReplGrpc.bindService(new ReplGrpcService(runtime), scheduler))
        .addService(DiagnosticsGrpc.bindService(diagnostics.grpc[F], scheduler))
        .addService(DeployServiceGrpc.bindService(new DeployGrpcService[F], scheduler))
        .build
    }

  def start[F[_]: FlatMap: Capture: Log](server: Server): F[Unit] =
    for {
      _ <- Capture[F].capture(server.start)
      _ <- Log[F].info("gRPC server started, listening on ")
    } yield ()

}
