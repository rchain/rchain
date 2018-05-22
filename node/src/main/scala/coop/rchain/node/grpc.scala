package coop.rchain.node

import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}

import scala.concurrent.Future
import cats._, cats.data._, cats.implicits._
import com.google.protobuf.empty.Empty
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc, DeployServiceResponse, DeployString}
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.node.model.repl._
import coop.rchain.node.model.diagnostics._
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import monix.execution.Scheduler
import com.google.protobuf.ByteString
import java.io.{Reader, StringReader}

import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics}
import coop.rchain.rholang.interpreter.errors.InterpreterError

object GrpcServer {

  def acquireServer[
      F[_]: Capture: Monad: MultiParentCasper: NodeDiscovery: JvmMetrics: NodeMetrics: Futurable](
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

    override def createBlock(e: Empty): Future[MaybeBlockMessage] =
      MultiParentCasper[F].createBlock.map(MaybeBlockMessage.apply).toFuture

    override def addBlock(b: BlockMessage): Future[Empty] =
      MultiParentCasper[F].addBlock(b).map(_ => Empty()).toFuture

    override def showBlock(q: BlockQuery): Future[BlockInfo] = {
      val dag = MultiParentCasper[F].blockDag
      val fullHash = dag.map(_.blockLookup.keys.find(h => {
        Base16.encode(h.toByteArray).startsWith(q.hash)
      }))
      val fBlock = fullHash.flatMap(hash => {
        hash.traverse(h => dag.map(_.blockLookup(h)))
      })

      fBlock
        .flatMap[BlockInfo] {
          case Some(block) =>
            val parents =
              ProtoUtil.parents(block).map(hash => "  " + Base16.encode(hash.toByteArray))
            val ps     = block.body.flatMap(_.postState)
            val tsHash = ps.map(_.tuplespace).getOrElse(ByteString.EMPTY)
            MultiParentCasper[F]
              .tsCheckpoint(tsHash)
              .map(maybeCheckPoint => {
                val tsDesc =
                  maybeCheckPoint
                    .map(checkpoint => {
                      val ts     = checkpoint.toTuplespace
                      val result = ts.storageRepr
                      ts.delete()
                      result
                    })
                    .getOrElse(s"Tuplespace hash ${Base16.encode(tsHash.toByteArray)} not found!")

                val blockDesc =
                  s"Parents:\n${parents.mkString("\n")}\n" +
                    s"Tuplespace:\n${tsDesc}\n"

                BlockInfo(blockDesc)
              })

          case None =>
            BlockInfo(s"Block with hash ${q.hash} not found!").pure[F]
        }
        .toFuture
    }
  }

  class ReplImpl(runtime: Runtime)(implicit scheduler: Scheduler) extends ReplGrpc.Repl {
    import RholangCLI.{buildNormalizedTerm, evaluate}

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
            evaluate(runtime.reducer, term).attempt.map {
              case Left(ie: InterpreterError) => s"Error: ${ie.toString}"
              case Left(ex)                   => s"Caught boxed exception: $ex"
              case Right(_) =>
                s"Storage Contents:\n ${StoragePrinter.prettyPrint(runtime.store)}"
            }
        }
        .map(ReplResponse(_))
        .executeAsync
        .runAsync

    def run(request: CmdRequest): Future[ReplResponse] =
      exec(new StringReader(request.line))

    def eval(request: EvalRequest): Future[ReplResponse] =
      exec(RholangCLI.reader(request.fileName))
  }
}
