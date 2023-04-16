package coop.rchain.node.effects

import cats.effect.Sync
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import coop.rchain.node.model._
import io.grpc.netty.NettyChannelBuilder
import io.grpc.{ManagedChannel, Metadata}

import java.io.{Closeable, FileNotFoundException}
import java.nio.file._
import java.util.concurrent.TimeUnit
import scala.io.Source
import scala.util.Using

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

class GrpcReplClient[F[_]: Async](host: String, port: Int, maxMessageSize: Int)
    extends ReplClient[F]
    with Closeable {

  private val channel: ManagedChannel =
    NettyChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(maxMessageSize)
      .usePlaintext()
      .build

  private val stub = Dispatcher.parallel[F].map(ReplFs2Grpc.stub(_, channel))

  def run(line: String): F[Either[Throwable, String]] =
    stub.use(
      _.run(CmdRequest(line), new Metadata())
        .map(_.output)
        .attempt
        .map(_.leftMap(processError))
    )

  def eval(
      fileNames: List[String],
      unmatchedSends: Boolean
  ): F[List[Either[Throwable, String]]] =
    fileNames.traverse(eval(_, unmatchedSends))

  def eval(fileName: String, printUnmatchedSendsOnly: Boolean): F[Either[Throwable, String]] = {
    val filePath = Paths.get(fileName)
    if (Files.exists(filePath))
      stub.use(
        _.eval(EvalRequest(readContent(filePath), printUnmatchedSendsOnly), new Metadata())
          .map(_.output)
          .attempt
          .map(_.leftMap(processError))
      )
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
