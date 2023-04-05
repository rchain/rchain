package coop.rchain.comm.transport

import cats.Applicative
import cats.effect.syntax.all._
import cats.effect.{Async, ConcurrentEffect, Sync}
import cats.syntax.all._
import coop.rchain.comm.CommError.{protocolException, CommErr}
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.transport.StreamObservable.StreamObservable
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream
import fs2.concurrent.SignallingRef
import io.grpc.netty._
import io.grpc.{CallOptions, ManagedChannel, Metadata}
import io.netty.handler.ssl.SslContext

import java.io.ByteArrayInputStream
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, _}
import scala.util._
import cats.effect.{Deferred, Ref}

/**
  * GRPC channel with a message buffer protecting it from resource exhaustion
  * @param grpcTransport underlying gRPC channel
  * @param buffer buffer implementing some kind of overflow policy
  */
final case class BufferedGrpcStreamChannel[F[_]](
    grpcTransport: ManagedChannel,
    buffer: StreamObservable[F],
    buferSubscriber: Stream[F, Unit]
)

class GrpcTransportClient[F[_]: Async: AsyncEffect: Log: Metrics](
    networkId: String,
    cert: String,
    key: String,
    maxMessageSize: Int,
    packetChunkSize: Int,
    clientQueueSize: Int,
    channelsMap: Ref[F, Map[PeerNode, Deferred[F, BufferedGrpcStreamChannel[F]]]]
) extends TransportLayer[F] {

  val DefaultSendTimeout: FiniteDuration = 5.seconds

  import coop.rchain.shared.RChainScheduler.ioScheduler
  val ioEC = ExecutionContext.fromExecutorService(ioScheduler)

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
        .executor(ioEC)
        .maxInboundMessageSize(maxMessageSize)
        .negotiationType(NegotiationType.TLS)
        .sslContext(clientSslContext)
        .intercept(new SslSessionClientInterceptor(networkId))
        .overrideAuthority(peer.id.toString)
        .build()
      buffer <- StreamObservable[F](peer, clientQueueSize, cache)

      buferSubscriber = buffer._2.evalMap(
        sMsg =>
          streamBlobFile(sMsg.key, peer, sMsg.sender)
            .guarantee(Sync[F].delay(cache.remove(sMsg.key)).void)
      )

      sig     <- SignallingRef(grpcChannel.isTerminated)
      channel = BufferedGrpcStreamChannel(grpcChannel, buffer, buferSubscriber.interruptWhen(sig))
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
            Log[F].info(
              s"Channel to peer ${peer.toAddress} is terminated, removing from connections map"
            ) >>
              channelsMap.update(_ - peer) >> getChannel(peer)
          else c.pure[F]
      _ <- Sync[F]
            .start(r.buferSubscriber.compile.drain)
            .onError {
              case err =>
                Log[F].error(s"Outbound gPRC channel to peer ${peer.toAddress} failed: $err") >>
                  channelsMap.update(_ - peer)
            }
            .onCancel { channelsMap.update(_ - peer) }
    } yield r

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration)(
      request: TransportLayerFs2Grpc[F, Metadata] => F[CommErr[A]]
  ): F[CommErr[A]] =
    (for {
      channel <- getChannel(peer)
      co      = CallOptions.DEFAULT.withDeadlineAfter(timeout.toMillis, MILLISECONDS)
      stub    = TransportLayerFs2Grpc.stub(channel.grpcTransport, co)
      result  <- request(stub)
    } yield result).attempt.map(_.fold(e => Left(protocolException(e)), identity))

  def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]] =
    withClient(peer, DefaultSendTimeout)(GrpcTransport.send(_, peer, msg))

  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]] =
    Stream
      .fromIterator(peers.iterator)
      .parEvalMapUnorderedProcBounded(send(_, msg))
      .compile
      .to(Seq)

  def stream(peers: Seq[PeerNode], blob: Blob): F[Unit] =
    Stream
      .fromIterator(peers.iterator)
      .parEvalMapUnorderedProcBounded { peer =>
        getChannel(peer).flatMap(_.buffer._1(blob))
      }
      .compile
      .drain

  private def streamBlobFile(
      key: String,
      peer: PeerNode,
      sender: PeerNode
  ): F[Unit] = {

    def timeout(packet: Packet): FiniteDuration =
      Math.max(packet.content.size().toLong * 5, DefaultSendTimeout.toMicros).micros

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
