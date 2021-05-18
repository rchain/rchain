package coop.rchain.comm.transport

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, ExitCode, Resource, Sync}
import cats.syntax.all._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.GrpcTransportReceiver.MessageBuffers
import coop.rchain.comm.{CommMetricsSource, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.shared._
import coop.rchain.shared.syntax._
import io.grpc.netty.GrpcSslContexts
import io.netty.handler.ssl._
import monix.execution.{Cancelable, Scheduler}

import java.io.ByteArrayInputStream
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.TrieMap
import scala.io.Source
import scala.util.{Left, Right}

trait TransportLayerServer[F[_]] {
  def handleReceive(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): F[Cancelable]
}

class GrpcTransportServer[F[_]: Monixable: Concurrent: RPConfAsk: Log: Metrics](
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
      .attempt
      .flatMap {
        case Right(sslContext) => sslContext.pure[F]
        case Left(t)           => Sync[F].raiseError[SslContext](t)
      }

  def handleReceive(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): F[Cancelable] = {

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

    for {
      serverSslContext <- serverSslContextTask
      messageBuffers   <- Ref.of[F, Map[PeerNode, Deferred[F, MessageBuffers]]](Map.empty)
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
  }
}

object GrpcTransportServer {
  def acquireServer[F[_]: Monixable: Concurrent: RPConfAsk: Log: Metrics](
      networkId: String,
      port: Int,
      certPath: Path,
      keyPath: Path,
      maxMessageSize: Int,
      maxStreamMessageSize: Long,
      parallelism: Int
  )(implicit mainScheduler: Scheduler): TransportServer[F] = {
    val cert = Resources.withResource(Source.fromFile(certPath.toFile))(_.mkString)
    val key  = Resources.withResource(Source.fromFile(keyPath.toFile))(_.mkString)
    new TransportServer[F](
      new GrpcTransportServer[F](
        networkId,
        port,
        cert,
        key,
        maxMessageSize,
        maxStreamMessageSize,
        parallelism
      )
    )
  }
}

class TransportServer[F[_]: Monixable: Concurrent](server: GrpcTransportServer[F]) {
  private val ref: AtomicReference[Option[Cancelable]] =
    new AtomicReference[Option[Cancelable]](None)

  def start(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): F[Unit] =
    ref.get() match {
      case Some(_) => ().pure[F]
      case _ =>
        server
          .handleReceive(dispatch, handleStreamed)
          .toTask
          .foreachL { cancelable =>
            ref
              .getAndSet(Some(cancelable))
              .fold(())(c => c.cancel())
          }
          .fromTask
    }

  def stop(): F[Unit] =
    ref
      .getAndSet(None)
      .fold(().pure[F])(c => Sync[F].delay(c.cancel()))
}
