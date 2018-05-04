package coop.rchain.node

import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}
import scala.concurrent.Future
import cats._, cats.data._, cats.implicits._
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc, DeployServiceResponse}
import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.node.rnode._
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import monix.execution.Scheduler
import com.google.protobuf.ByteString

import java.io.{Reader, StringReader}

object GrpcServer {

  def acquireServer(port: Int, runtime: Runtime)(implicit scheduler: Scheduler): GrpcServerBuilder =
    GrpcServerBuilder(scheduler, Repl.DefaultScheduler, port, runtime)

  def start[F[_]: FlatMap: Capture: Log](server: Server): F[Unit] =
    for {
      _ <- Capture[F].capture(server.start)
      _ <- Log[F].info("gRPC server started, listening on ")
    } yield ()
}

case class GrpcServerBuilder private (
    mainScheduler: Scheduler,
    replScheduler: Scheduler,
    port: Int,
    runtime: Runtime
) {

  def withReplScheduler(scheduler: Scheduler): GrpcServerBuilder = copy(replScheduler = scheduler)

  def build[F[_]: Capture: Functor: MultiParentCasper: NodeDiscovery: Futurable]: F[Server] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port).executor()
        .addService(ReplGrpc.bindService(new ReplImpl(runtime, replScheduler), mainScheduler))
        .addService(DiagnosticsGrpc.bindService(new DiagnosticsImpl[F], mainScheduler))
        .addService(DeployServiceGrpc.bindService(new DeployImpl[F], mainScheduler))
        .build
    }
}

private class DiagnosticsImpl[F[_]: Functor: NodeDiscovery: Futurable]
    extends DiagnosticsGrpc.Diagnostics {
  def listPeers(request: ListPeersRequest): Future[Peers] =
    NodeDiscovery[F].peers.map { ps =>
      Peers(ps.map(p =>
        Peer(p.endpoint.host, p.endpoint.udpPort, ByteString.copyFrom(p.id.key.toArray))))
    }.toFuture
}

private class DeployImpl[F[_]: Functor: MultiParentCasper: Futurable]
    extends DeployServiceGrpc.DeployService {
  def doDeploy(d: Deploy): Future[DeployServiceResponse] = {
    val f = for {
      _ <- MultiParentCasper[F].deploy(d)
    } yield DeployServiceResponse(true)

    f.toFuture
  }
}

object Repl {
  // This Scheduler is intended for internal use of ReplImpl. Don't use it anywhere else!
  private[node] val DefaultScheduler: Scheduler = Scheduler.fixedPool("rholang-cli", poolSize = 1)
}

private class ReplImpl(runtime: Runtime, scheduler: Scheduler) extends ReplGrpc.Repl {
  import RholangCLI.{buildNormalizedTerm, evaluate}

  private[this] def exec(reader: Reader): Task[ReplResponse] =
    buildNormalizedTerm(reader) match {
      case Left(er) =>
        Task.pure(ReplResponse(s"Error: $er"))
      case Right(term) =>
        evaluate(runtime.reducer, term).attempt
          .map {
            case Left(ex) => s"Caught boxed exception: $ex"
            case Right(_) =>
              s"Storage Contents:\n ${StoragePrinter.prettyPrint(runtime.store)}"
          }
          .map(ReplResponse(_))
    }

  private[this] def scheduleExec(reader: Reader): Future[ReplResponse] =
    exec(reader).executeAsync.runAsync(scheduler)

  def run(request: CmdRequest): Future[ReplResponse] =
    scheduleExec(new StringReader(request.line))

  def eval(request: EvalRequest): Future[ReplResponse] =
    scheduleExec(RholangCLI.reader(request.fileName))
}
