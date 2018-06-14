package coop.rchain.node

import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}

import scala.concurrent.Future
import cats._
import cats.data._
import cats.implicits._
import com.google.protobuf.empty.Empty
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc, DeployServiceResponse, DeployString}
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.catscontrib._
import Catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.node.model.repl._
import coop.rchain.node.model.diagnostics._
import coop.rchain.rholang.interpreter.{PrettyPrinter, RholangCLI, Runtime, errors}
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
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter._

object GrpcServer {

  def acquireServer[
      F[_]: Capture: Monad: MultiParentCasper: NodeDiscovery: StoreMetrics: JvmMetrics: NodeMetrics: Futurable](
      port: Int,
      runtime: Runtime)(implicit scheduler: Scheduler): F[Server] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port)
        .addService(ReplGrpc.bindService(new ReplImpl(runtime), scheduler))
        .addService(DiagnosticsGrpc.bindService(diagnostics.grpc[F], scheduler))
        .addService(DeployServiceGrpc.bindService(new DeployImpl[F], scheduler))
        .build
    }

  def start[F[_]: FlatMap: Capture: Log](server: Server): F[Unit] =
    for {
      _ <- Capture[F].capture(server.start)
      _ <- Log[F].info("gRPC server started, listening on ")
    } yield ()

  class DeployImpl[F[_]: Monad: MultiParentCasper: Futurable]
      extends DeployServiceGrpc.DeployService {
    override def doDeploy(d: DeployString): Future[DeployServiceResponse] =
      InterpreterUtil.mkTerm(d.term) match {
        case Right(term) =>
          val deploy = Deploy(
            user = d.user,
            nonce = d.nonce,
            term = Some(term),
            sig = d.sig
          )
          val f = for {
            _ <- MultiParentCasper[F].deploy(deploy)
          } yield DeployServiceResponse(true, "Success!")

          f.toFuture

        case Left(err) =>
          Future.successful(DeployServiceResponse(false, s"Error in parsing term: \n$err"))
      }

    override def createBlock(e: Empty): Future[MaybeBlockMessage] = BlockAPI.createBlock[F].toFuture

    override def addBlock(b: BlockMessage): Future[Empty] = BlockAPI.addBlock[F](b).toFuture

    override def showBlock(q: BlockQuery): Future[BlockQueryResponse] =
      BlockAPI.getBlockQueryResponse[F](q).toFuture

    override def showBlocks(e: Empty): Future[BlocksResponse] =
      BlockAPI.getBlocksResponse[F].toFuture
  }

  class ReplImpl(runtime: Runtime)(implicit scheduler: Scheduler) extends ReplGrpc.Repl {
    def exec(reader: Reader): Future[ReplResponse] =
      Task
        .coeval(buildNormalizedTerm(reader))
        .attempt
        .flatMap {
          case Left(er) =>
            er match {
              case _: InterpreterError => Task.now(s"Error: ${er.toString}")
              case th: Throwable       => Task.now(s"Error: $th")
            }
          case Right(term) =>
            runEvaluate(runtime, term).attempt.map {
              case Left(ex) => s"Caught boxed exception: $ex"
              case Right(errors) => {
                val errorStr =
                  if (errors.isEmpty)
                    ""
                  else
                    errors
                      .map(_.toString())
                      .mkString("Errors received during evaluation:\n", "\n", "\n")
                s"${errorStr}Storage Contents:\n ${StoragePrinter.prettyPrint(runtime.space.store)}"
              }
            }
        }
        .map(ReplResponse(_))
        .executeAsync
        .runAsync

    def run(request: CmdRequest): Future[ReplResponse] =
      exec(new StringReader(request.line))

    def eval(request: EvalRequest): Future[ReplResponse] =
      exec(new StringReader(request.program))

    def runEvaluate(runtime:Runtime, term : Par) : Task[Vector[errors.InterpreterError]] =
      for {
        _ <- Task.now(printNormalizedTerm(term))
        result <- evaluate(runtime, term)
      } yield (result)

    private def printNormalizedTerm(normalizedTerm: Par): Unit = {
      Console.println("\nEvaluating:")
      Console.println(PrettyPrinter().buildString(normalizedTerm))
    }
  }
}
