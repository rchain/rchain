package coop.rchain.node

import java.io.File

import scala.io.Source

import coop.rchain.node.model.repl._
import coop.rchain.shared.Resources._

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
    val file = new File(fileName)
    if (file.exists()) {
      withResource(Source.fromFile(file)) { source =>
        blockingStub.eval(EvalRequest(source.getLines.mkString("\n"))).output
      }
    } else {
      s"File $fileName not found"
    }
  }
}
