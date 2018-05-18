package coop.rchain.node

import coop.rchain.node.model.repl._

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task

trait ReplService[F[_]] {
  def run(line: String): F[String]
  def eval(line: String): F[String]
}

object ReplService {
  def apply[F[_]](implicit ev: ReplService[F]): ReplService[F] = ev
}

class GrpcReplService(host: String, port: Int) extends ReplService[Task] {

  private val channel: ManagedChannel =
    ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build
  private val blockingStub = ReplGrpc.blockingStub(channel)

  def run(line: String): Task[String] = Task.delay {
    blockingStub.run(CmdRequest(line)).output
  }

  def eval(fileName: String): Task[String] = Task.delay {
    blockingStub.eval(EvalRequest(fileName)).output
  }
}
