package coop.rchain.node.effects

import java.nio.file.{Files, Path, Paths}

import cats.implicits._
import coop.rchain.node.model.repl._
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task

trait ReplClient[F[_]] {
  def run(line: String): F[String]
  def eval(fileNames: List[String]): F[String]
}

object ReplClient {
  def apply[F[_]](implicit ev: ReplClient[F]): ReplClient[F] = ev
}

class GrpcReplClient(host: String, port: Int) extends ReplClient[Task] {

  private val channel: ManagedChannel =
    ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build
  private val blockingStub = ReplGrpc.blockingStub(channel)

  def run(line: String): Task[String] = Task.delay {
    blockingStub.run(CmdRequest(line)).output
  }
  def eval(fileNames: List[String]): Task[String] =
    fileNames
      .traverse(eval)
      .map(_.mkString("\n"))

  def eval(fileName: String): Task[String] = Task.delay {
    val filePath = Paths.get(fileName)
    if (Files.exists(filePath)) {
      blockingStub.eval(EvalRequest(readContent(filePath))).output
    } else {
      s"File $fileName not found"
    }
  }

  private def readContent(filePath: Path): String =
    new String(Files.readAllBytes(filePath))
}
