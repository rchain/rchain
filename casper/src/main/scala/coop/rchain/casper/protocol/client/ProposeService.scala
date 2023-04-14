package coop.rchain.casper.protocol.client

import cats.effect.{Async, Sync}
import cats.effect.std.Dispatcher
import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.propose.v1._
import coop.rchain.models.either.implicits._
import coop.rchain.shared.syntax._
import io.grpc.netty.NettyChannelBuilder
import io.grpc.{ManagedChannel, ManagedChannelBuilder, Metadata}

import java.io.Closeable
import java.util.concurrent.TimeUnit

trait ProposeService[F[_]] {
  def propose(isAsync: Boolean): F[Either[Seq[String], String]]
}

object ProposeService {
  def apply[F[_]](implicit ev: ProposeService[F]): ProposeService[F] = ev
}

class GrpcProposeService[F[_]: Async](host: String, port: Int, maxMessageSize: Int)
    extends ProposeService[F]
    with Closeable {

  private val channel: ManagedChannel =
    NettyChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(maxMessageSize)
      .usePlaintext()
      .build

  private val stub = ProposeServiceFs2Grpc.stub(channel)

  def propose(isAsync: Boolean): F[Either[Seq[String], String]] =
    stub
      .propose(ProposeQuery(isAsync), new Metadata)
      .toEitherF(
        _.message.error,
        _.message.result
      )

  def proposeResult: F[Either[Seq[String], String]] =
    stub
      .proposeResult(ProposeResultQuery(), new Metadata)
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
