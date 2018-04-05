package coop.rchain.node

import io.grpc.{Server, ServerBuilder}
import scala.concurrent.{ExecutionContext, Future}
import cats._, cats.data._, cats.implicits._
import coop.rchain.node.repl._
import coop.rchain.rholang.interpreter.Runtime

class GrpcServer(executionContext: ExecutionContext, port: Int, runtime: Runtime) { self =>

  var server: Option[Server] = None

  def start(): Unit = {
    server = ServerBuilder
      .forPort(port)
      .addService(ReplGrpc.bindService(new ReplImpl, executionContext))
      .build
      .start
      .some

    println("Server started, listening on " + port)
    sys.addShutdownHook {
      System.err.println("*** shutting down gRPC server since JVM is shutting down")
      self.stop()
      System.err.println("*** server shut down")
    }
  }

  def stop(): Unit =
    server.foreach(_.shutdown())

  class ReplImpl extends ReplGrpc.Repl {
    def run(request: ReplRequest): Future[ReplResponse] =
      Future.successful(ReplResponse(s"running ${request.line}"))
  }

}
