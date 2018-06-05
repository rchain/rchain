package coop.rchain.node

import java.io.File

import scala.io.Source

import coop.rchain.node.model.repl._
import coop.rchain.shared.Resources._

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task

trait ReplService[F[_]] {
  def run(line: String): F[String]
  def eval(fileNames: List[String]): F[String]
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

  def eval(fileNames: List[String]): Task[String] = Task.delay {
    val (existing, missing) =
      fileNames
        .map(new File(_))
        .partition(_.exists())

    if (missing.isEmpty) {
      val sources = existing.map(f => Source.fromFile(f))

      try {
        val content = sources.flatMap(_.getLines()).mkString("\n")

        blockingStub.eval(EvalRequest(content)).output
      } finally {
        sources.foreach(_.close())
      }
    } else {
      s"Files not found: ${missing.map(_.getName)}"
    }
  }
}
