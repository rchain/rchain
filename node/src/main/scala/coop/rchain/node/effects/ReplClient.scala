package coop.rchain.node.effects

import java.io.{Closeable, FileNotFoundException}
import java.nio.file._
import java.util.concurrent.TimeUnit

import cats.effect.Sync

import scala.io.Source
import cats.syntax.all._
import coop.rchain.monix.Monixable
import coop.rchain.node.model.repl._
import coop.rchain.shared.Resources
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task
import coop.rchain.shared.syntax._

trait ReplClient[F[_]] {
  def run(line: String): F[Either[Throwable, String]]
  def eval(
      fileNames: List[String],
      printUnmatchedSendsOnly: Boolean
  ): F[List[Either[Throwable, String]]]
}

object ReplClient {
  def apply[F[_]](implicit ev: ReplClient[F]): ReplClient[F] = ev
}

class GrpcReplClient[F[_]: Monixable: Sync](host: String, port: Int, maxMessageSize: Int)
    extends ReplClient[F]
    with Closeable {

  private val channel: ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(maxMessageSize)
      .usePlaintext()
      .build

  private val stub = ReplGrpcMonix.stub(channel)

  def run(line: String): F[Either[Throwable, String]] =
    stub
      .run(CmdRequest(line))
      .fromTask
      .map(_.output)
      .attempt
      .map(_.leftMap(processError))

  def eval(
      fileNames: List[String],
      unmatchedSends: Boolean
  ): F[List[Either[Throwable, String]]] = {
    import cats.instances.list._

    fileNames.traverse(eval(_, unmatchedSends))
  }

  def eval(fileName: String, printUnmatchedSendsOnly: Boolean): F[Either[Throwable, String]] = {
    val filePath = Paths.get(fileName)
    if (Files.exists(filePath))
      stub
        .eval(EvalRequest(readContent(filePath), printUnmatchedSendsOnly))
        .fromTask
        .map(_.output)
        .attempt
        .map(_.leftMap(processError))
    else Sync[F].delay(new FileNotFoundException("File not found").asLeft)
  }

  private def readContent(filePath: Path): String =
    Using.resource(Source.fromFile(filePath.toFile))(_.mkString)

  private def processError(t: Throwable): Throwable =
    Option(t.getCause).getOrElse(t)

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  override def close(): Unit = {
    val terminated = channel.shutdown().awaitTermination(10, TimeUnit.SECONDS)
    if (!terminated) {
      println(
        "warn: did not shutdown after 10 seconds, retrying with additional 10 seconds timeout"
      )
      channel.awaitTermination(10, TimeUnit.SECONDS)
    }
  }
}
