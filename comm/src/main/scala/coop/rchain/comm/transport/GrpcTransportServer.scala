package coop.rchain.comm.transport

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Resource, Sync, Timer}
import cats.syntax.all._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.GrpcTransportReceiver.MessageBuffers
import coop.rchain.comm.{CommMetricsSource, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.shared._
import io.grpc.netty.GrpcSslContexts
import io.netty.handler.ssl._
import monix.execution.Scheduler

import java.io.ByteArrayInputStream
import java.nio.file.Path
import scala.collection.concurrent.TrieMap
import scala.io.Source
import scala.util.{Left, Right, Using}

trait TransportLayerServer[F[_]] {
  def resource(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): Resource[F, Unit]
}

object TransportLayerServer {
  def apply[F[_]](grpcServer: GrpcTransportServer[F]): TransportLayerServer[F] =
    // Use trait directly to allow IntelliJ to find implementation
    new TransportLayerServer[F] {
      override def resource(
          dispatch: Protocol => F[CommunicationResponse],
          handleStreamed: Blob => F[Unit]
      ): Resource[F, Unit] = grpcServer.resource(dispatch, handleStreamed)
    }
}

class GrpcTransportServer[F[_]: Monixable: Concurrent: RPConfAsk: Log: Metrics: Timer](
    networkId: String,
    port: Int,
    cert: String,
    key: String,
    maxMessageSize: Int,
    maxStreamMessageSize: Long,
    parallelism: Int
)(implicit mainScheduler: Scheduler)
    extends TransportLayerServer[F] {
  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  // Cache to store received partial data (streaming packets)
  private val cache = TrieMap[String, Array[Byte]]()

  private val serverSslContextTask: F[SslContext] =
    Sync[F]
      .delay {
        GrpcSslContexts
          .configure(SslContextBuilder.forServer(certInputStream, keyInputStream))
          .trustManager(HostnameTrustManagerFactory.Instance)
          .clientAuth(ClientAuth.REQUIRE)
          .build()
      }

  override def resource(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): Resource[F, Unit] = {

    val dispatchSend: Send => F[Unit] = s =>
      dispatch(s.msg)
        .logOnError("Sending gRPC message failed.") >>
        Metrics[F].incrementCounter("dispatched.messages")

    val dispatchBlob: StreamMessage => F[Unit] =
      msg =>
        (StreamHandler.restore(msg, cache) >>= {
          case Left(ex) =>
            Log[F].error("Could not restore data from file while handling stream", ex)
          case Right(blob) =>
            handleStreamed(blob)
        }) >> Metrics[F].incrementCounter("dispatched.packets")

    val cancelable = for {
      serverSslContext <- serverSslContextTask
      messageBuffers   <- Ref.of[F, Map[PeerNode, Deferred[F, MessageBuffers[F]]]](Map.empty)
      receiver <- GrpcTransportReceiver.create(
                   networkId: String,
                   port,
                   serverSslContext,
                   maxMessageSize,
                   maxStreamMessageSize,
                   messageBuffers,
                   (dispatchSend, dispatchBlob),
                   parallelism = parallelism,
                   cache
                 )
    } yield receiver

    Resource.make(cancelable)(c => Sync[F].delay(c.cancel())).as(())
  }
}

object GrpcTransportServer {

  def acquireServer[F[_]: Monixable: Concurrent: RPConfAsk: Log: Metrics: Timer](
      networkId: String,
      port: Int,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      maxStreamMessageSize: Long,
      parallelism: Int
  )(
      implicit mainScheduler: Scheduler
  ): TransportLayerServer[F] = {
    val cert = Using.resource(Source.fromFile(certPath.toFile))(_.mkString)
    val key  = Using.resource(Source.fromFile(keyPath.toFile))(_.mkString)
    val server = new GrpcTransportServer[F](
      networkId,
      port,
      cert,
      key,
      maxMessageSize,
      maxStreamMessageSize,
      parallelism
    )
    TransportLayerServer(server)
  }

}
