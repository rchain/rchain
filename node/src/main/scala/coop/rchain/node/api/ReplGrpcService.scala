package coop.rchain.node.api

import coop.rchain.node.diagnostics
import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}

import scala.concurrent.Future
import cats._, cats.data._, cats.implicits._
import com.google.protobuf.empty.Empty
import coop.rchain.casper.{MultiParentCasper}
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

import coop.rchain.casper.api.BlockAPI
import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics, StoreMetrics}
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter._, Interpreter._, storage.StoragePrinter

private[api] class ReplGrpcService(runtime: Runtime)(implicit scheduler: Scheduler)
    extends ReplGrpc.Repl {
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

  def runEvaluate(runtime: Runtime, term: Par): Task[Vector[errors.InterpreterError]] =
    for {
      _      <- Task.now(printNormalizedTerm(term))
      result <- evaluate(runtime, term)
    } yield (result)

  private def printNormalizedTerm(normalizedTerm: Par): Unit = {
    Console.println("\nEvaluating:")
    Console.println(PrettyPrinter().buildString(normalizedTerm))
  }
}
