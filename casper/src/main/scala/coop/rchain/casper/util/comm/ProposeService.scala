package coop.rchain.casper.util.comm

import java.io.Closeable
import java.util.concurrent.TimeUnit

import cats.effect.Sync
import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.models.either.implicits._
import coop.rchain.monix.Monixable
import coop.rchain.shared.syntax._
import io.grpc.{ManagedChannel, ManagedChannelBuilder}

import scala.util.Either

trait ProposeService[F[_]] {
  def propose(printUnmatchedSends: Boolean): F[Either[Seq[String], String]]
}

object ProposeService {
  def apply[F[_]](implicit ev: ProposeService[F]): ProposeService[F] = ev
}

class GrpcProposeService[F[_]: Monixable: Sync](host: String, port: Int, maxMessageSize: Int)
    extends ProposeService[F]
    with Closeable {

  private val channel: ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(maxMessageSize)
      .usePlaintext()
      .build

  private val stub = ProposeServiceV1GrpcMonix.stub(channel)

  def propose(printUnmatchedSends: Boolean): F[Either[Seq[String], String]] =
    stub
      .propose(PrintUnmatchedSendsQuery(printUnmatchedSends))
      .fromTask
      .toEitherF(
        _.message.error,
        _.message.result
      )

  def proposeResult: F[Either[Seq[String], String]] =
    stub
      .proposeResult(ProposeResultQuery())
      .fromTask
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
