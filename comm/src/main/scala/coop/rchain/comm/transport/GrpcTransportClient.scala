package coop.rchain.comm.transport

import java.io.ByteArrayInputStream

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.syntax.all._
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Applicative, Parallel}
import coop.rchain.comm.CommError.{protocolException, CommErr}
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.grpc.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import io.grpc.ManagedChannel
import io.grpc.netty._
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.Ack.Continue
import monix.execution.{Cancelable, CancelableFuture, Scheduler}

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.{FiniteDuration, _}
import scala.util._

/**
  * GRPC channel with a message buffer protecting it from resource exhaustion
  * @param grpcTransport underlying gRPC channel
  * @param buffer buffer implementing some kind of overflow policy
  */
final case class BufferedGrpcStreamChannel[F[_]](
    grpcTransport: ManagedChannel,
    buffer: StreamObservable[F],
    buferSubscriber: Cancelable
)

class GrpcTransportClient[F[_]: Monixable: Concurrent: Parallel: Log: Metrics](
    networkId: String,
    cert: String,
    key: String,
    maxMessageSize: Int,
    packetChunkSize: Int,
    clientQueueSize: Int,
    networkTimeout: FiniteDuration = 5.seconds,
    channelsMap: Ref[F, Map[PeerNode, Deferred[F, BufferedGrpcStreamChannel[F]]]],
    ioScheduler: Scheduler
)(implicit scheduler: Scheduler)
    extends TransportLayer[F] {

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  // Cache to store received partial data (streaming packets)
  private val cache = TrieMap[String, Array[Byte]]()

  private val clientSslContextTask: F[SslContext] =
    Sync[F]
      .delay {
        GrpcSslContexts.forClient
          .trustManager(HostnameTrustManagerFactory.Instance)
          .keyManager(certInputStream, keyInputStream)
          .build
      }
      .attempt
      .flatMap {
        case Right(sslContext) => sslContext.pure[F]
        case Left(t)           => t.raiseError[F, SslContext]
      }

  private def createChannel(peer: PeerNode): F[BufferedGrpcStreamChannel[F]] =
    for {
      _                <- Log[F].info(s"Creating new channel to peer ${peer.toAddress}")
      clientSslContext <- clientSslContextTask
      grpcChannel = NettyChannelBuilder
        .forAddress(peer.endpoint.host, peer.endpoint.tcpPort)
        .executor(ioScheduler)
        .maxInboundMessageSize(maxMessageSize)
        .negotiationType(NegotiationType.TLS)
        .sslContext(clientSslContext)
        .intercept(new SslSessionClientInterceptor(networkId))
        .overrideAuthority(peer.id.toString)
        .build()
      buffer = new StreamObservable[F](peer, clientQueueSize, cache)

      buferSubscriber = buffer.subscribe(
        sMsg =>
          streamBlobFile(sMsg.key, peer, sMsg.sender)
            .guarantee(Sync[F].delay(cache.remove(sMsg.key)).void)
            .toTask
            .runToFuture >> Continue.pure[CancelableFuture]
      )

      channel = BufferedGrpcStreamChannel(grpcChannel, buffer, buferSubscriber)
    } yield channel

  private def getChannel(peer: PeerNode): F[BufferedGrpcStreamChannel[F]] =
    for {
      cDefNew <- Deferred[F, BufferedGrpcStreamChannel[F]]
      ret <- channelsMap.modify[(Deferred[F, BufferedGrpcStreamChannel[F]], Boolean)] { chMap =>
              val noCh = !chMap.exists(c => c._1 equals peer)
              if (noCh) {
                (chMap + (peer -> cDefNew), (cDefNew, true))
              } else {
                (chMap, (chMap(peer), false))
              }
            }
      (cDef, newChannel) = ret
      _                  <- Applicative[F].whenA(newChannel)(createChannel(peer) >>= cDef.complete)
      c                  <- cDef.get
      // In case underlying gRPC transport is terminated - clean resources,
      // remove current record and try one more time
      r <- if (c.grpcTransport.isTerminated)
            Sync[F].delay(c.buferSubscriber.cancel()) >>
              Log[F].info(
                s"Channel to peer ${peer.toAddress} is terminated, removing from connections map"
              ) >>
              channelsMap.update(_ - peer) >> getChannel(peer)
          else c.pure[F]
    } yield r

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration)(
      request: RoutingGrpcMonix.TransportLayer => F[CommErr[A]]
  ): F[CommErr[A]] =
    (for {
      channel <- getChannel(peer)
      stub <- Sync[F].delay(
               RoutingGrpcMonix.stub(channel.grpcTransport).withDeadlineAfter(timeout)
             )
      result <- request(stub)
      _      <- Task.unit.asyncBoundary.fromTask // return control to caller thread
    } yield result).attempt.map(_.fold(e => Left(protocolException(e)), identity))

  def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]] =
    withClient(peer, networkTimeout)(GrpcTransport.send(_, peer, msg))

  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]] = {
    import cats.instances.list._

    peers.toList.parTraverse(send(_, msg)).map(_.toSeq)
  }

  def stream(peer: PeerNode, blob: Blob): F[Unit] =
    getChannel(peer).flatMap(_.buffer.enque(blob))

  def stream(peers: Seq[PeerNode], blob: Blob): F[Unit] = {
    import cats.instances.list._

    peers.toList.parTraverse_(stream(_, blob))
  }

  private def streamBlobFile(
      key: String,
      peer: PeerNode,
      sender: PeerNode
  ): F[Unit] = {

    def timeout(packet: Packet): FiniteDuration =
      Math.max(packet.content.size().toLong * 5, networkTimeout.toMicros).micros

    PacketOps.restore[F](key, cache) >>= {
      case Right(packet) =>
        Log[F].debug(
          s"Attempting to stream packet to $peer with timeout: ${timeout(packet).toMillis}ms"
        ) >>
          withClient(peer, timeout(packet)) { transport =>
            GrpcTransport.stream(transport, peer, networkId, Blob(sender, packet), packetChunkSize)
          }.flatMap {
            case Left(error) =>
              Log[F].debug(
                s"Error while streaming packet to $peer (timeout: ${timeout(packet).toMillis}ms): ${error.message}"
              )
            case Right(_) => Log[F].debug(s"Streamed packet $key to $peer")
          }
      case Left(error) =>
        Log[F].error(s"Error while streaming packet $key to $peer: ${error.message}")
    }
  }
}
