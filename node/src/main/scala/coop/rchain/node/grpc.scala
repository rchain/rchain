package coop.rchain.node

import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}
import scala.concurrent.{ExecutionContext, Future}
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.node.rnode._
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.execution.Scheduler

import java.io.{Reader, StringReader}

object GrpcServer {

  def acquireServer[F[_]: Capture](executionContext: ExecutionContext,
                                   port: Int,
                                   runtime: Runtime): F[Server] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port)
        .addService(ReplGrpc.bindService(new ReplImpl(runtime), executionContext))
        .build
    }

  def start[F[_]: FlatMap: Capture: Log](server: Server): F[Unit] =
    for {
      _ <- Capture[F].capture(server.start)
      _ <- Log[F].info("gRPC server started, listening on ")
    } yield ()

  class ReplImpl(runtime: Runtime) extends ReplGrpc.Repl {
    import RholangCLI._
    // TODO we need to handle this better
    import monix.execution.Scheduler.Implicits.global

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
