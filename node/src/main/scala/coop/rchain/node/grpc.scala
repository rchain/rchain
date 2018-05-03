package coop.rchain.node

import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}
import scala.concurrent.{ExecutionContext, Future}
import cats._, cats.data._, cats.implicits._
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc, DeployServiceResponse}
import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.node.rnode._
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.execution.Scheduler
import com.google.protobuf.ByteString

import java.io.{Reader, StringReader}

object GrpcServer {

  def acquireServer[F[_]: Capture: Functor: MultiParentCasper: NodeDiscovery: Futurable](
      executionContext: ExecutionContext,
      port: Int,
      runtime: Runtime): F[Server] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port)
        .addService(ReplGrpc.bindService(new ReplImpl(runtime), executionContext))
        .addService(DiagnosticsGrpc.bindService(new DiagnosticsImpl[F], executionContext))
        .addService(DeployServiceGrpc.bindService(new DeployImpl[F], executionContext))
        .build
    }

  def start[F[_]: FlatMap: Capture: Log](server: Server): F[Unit] =
    for {
      _ <- Capture[F].capture(server.start)
      _ <- Log[F].info("gRPC server started, listening on ")
    } yield ()

  class DiagnosticsImpl[F[_]: Functor: NodeDiscovery: Futurable]
      extends DiagnosticsGrpc.Diagnostics {
    def listPeers(request: ListPeersRequest): Future[Peers] =
      NodeDiscovery[F].peers.map { ps =>
        Peers(ps.map(p =>
          Peer(p.endpoint.host, p.endpoint.udpPort, ByteString.copyFrom(p.id.key.toArray))))
      }.toFuture
  }

  class DeployImpl[F[_]: Functor: MultiParentCasper: Futurable]
      extends DeployServiceGrpc.DeployService {
    def doDeploy(d: Deploy): Future[DeployServiceResponse] = {
      val f = for {
        _ <- MultiParentCasper[F].deploy(d)
      } yield DeployServiceResponse(true)

      f.toFuture
    }
  }

  class ReplImpl(runtime: Runtime) extends ReplGrpc.Repl {
    import RholangCLI._
    // TODO we need to handle this better
    import monix.execution.Scheduler
    import monix.execution.schedulers.SchedulerService

    implicit val io: SchedulerService = Scheduler.io("repl-io")

    def exec(reader: Reader): Future[ReplResponse] = buildNormalizedTerm(reader) match {
      case Left(er) =>
        Future.successful(ReplResponse(s"Error: $er"))
      case Right(term) =>
        evaluate(runtime.reducer, term).attempt
          .map {
            case Left(ex) => s"Caught boxed exception: $ex"
            case Right(_) => s"Storage Contents:\n ${StoragePrinter.prettyPrint(runtime.store)}"
          }
          .map(ReplResponse(_))
          .runAsync
    }

    def run(request: CmdRequest): Future[ReplResponse] =
      exec(new StringReader(request.line))

    def eval(request: EvalRequest): Future[ReplResponse] =
      exec(RholangCLI.reader(request.fileName))
  }
}
