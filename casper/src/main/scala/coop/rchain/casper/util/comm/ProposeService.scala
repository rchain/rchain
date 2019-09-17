package coop.rchain.casper.util.comm

import java.io.Closeable
import java.util.concurrent.TimeUnit

import scala.util.Either

import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.propose.ProposeServiceV2GrpcMonix
import coop.rchain.models.either.implicits._

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task

trait ProposeService[F[_]] {
  def propose(printUnmatchedSends: Boolean): F[Either[Seq[String], String]]
}

object ProposeService {
  def apply[F[_]](implicit ev: ProposeService[F]): ProposeService[F] = ev
}

class GrpcProposeService(host: String, port: Int, maxMessageSize: Int)
    extends ProposeService[Task]
    with Closeable {

  private val channel: ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(maxMessageSize)
      .usePlaintext()
      .build

  private val stub = ProposeServiceV2GrpcMonix.stub(channel)

  def propose(printUnmatchedSends: Boolean): Task[Either[Seq[String], String]] =
    stub
      .propose(PrintUnmatchedSendsQuery(printUnmatchedSends))
      .toEitherF(
        _.message.error,
        _.message.result
      )

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
