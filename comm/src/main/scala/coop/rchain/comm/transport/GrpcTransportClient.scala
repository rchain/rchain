package coop.rchain.comm.transport

import java.io.ByteArrayInputStream
import java.nio.file.Path

import cats.Applicative
import cats.effect.concurrent.{Deferred, Ref}

import scala.concurrent.duration.{FiniteDuration, _}
import scala.util._
import cats.implicits._
import coop.rchain.catscontrib.ski.kp
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.CommError.{protocolException, CommErr}
import coop.rchain.grpc.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, UncaughtExceptionLogger}
import coop.rchain.shared.PathOps.PathDelete
import io.grpc.ManagedChannel
import io.grpc.netty._
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.Ack.Continue
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.reactive.Observable

/**
  * GRPC channel with a message buffer protecting it from resource exhaustion
  * @param grpcTransport underlying gRPC channel
  * @param buffer buffer implementing some kind of overflow policy
  */
final case class BufferedGrpcStreamChannel(
    grpcTransport: ManagedChannel,
    buffer: StreamObservable,
    buferSubscriber: Cancelable
)

class GrpcTransportClient(
    networkId: String,
    cert: String,
    key: String,
    maxMessageSize: Int,
    packetChunkSize: Int,
    tempFolder: Path,
    clientQueueSize: Int,
    channelsMap: Ref[Task, Map[PeerNode, Deferred[Task, BufferedGrpcStreamChannel]]]
)(
    implicit scheduler: Scheduler,
    val log: Log[Task],
    val metrics: Metrics[Task]
) extends TransportLayer[Task] {

  val DefaultSendTimeout: FiniteDuration = 5.seconds

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  private val clientSslContextTask: Task[SslContext] =
    Task
      .evalOnce {
        GrpcSslContexts.forClient
          .trustManager(HostnameTrustManagerFactory.Instance)
          .keyManager(certInputStream, keyInputStream)
          .build
      }
      .attempt
      .flatMap {
        case Right(sslContext) => sslContext.pure[Task]
        case Left(t)           => Task.raiseError[SslContext](t)
      }

  private def createChannel(peer: PeerNode): Task[BufferedGrpcStreamChannel] =
    for {
      _                <- log.info(s"Creating new channel to peer ${peer.toAddress}")
      clientSslContext <- clientSslContextTask
      grpcChannel = NettyChannelBuilder
        .forAddress(peer.endpoint.host, peer.endpoint.tcpPort)
        .executor(scheduler)
        .maxInboundMessageSize(maxMessageSize)
        .negotiationType(NegotiationType.TLS)
        .sslContext(clientSslContext)
        .intercept(new SslSessionClientInterceptor(networkId))
        .overrideAuthority(peer.id.toString)
        .build()
      buffer = new StreamObservable(peer, clientQueueSize, tempFolder)

      buferSubscriber = buffer.subscribe(
        sMsg =>
          streamBlobFile(sMsg.path, peer, sMsg.sender)
            .guarantee(sMsg.path.deleteSingleFile[Task])
            .runToFuture >> Continue.pure[CancelableFuture]
      )

      channel = BufferedGrpcStreamChannel(grpcChannel, buffer, buferSubscriber)
    } yield channel

  private def getChannel(peer: PeerNode): Task[BufferedGrpcStreamChannel] =
    for {
      cDefNew <- Deferred[Task, BufferedGrpcStreamChannel]
      ret <- channelsMap.modify[(Deferred[Task, BufferedGrpcStreamChannel], Boolean)] { chMap =>
              val noCh = !chMap.exists(c => c._1 equals peer)
              if (noCh) {
                (chMap + (peer -> cDefNew), (cDefNew, true))
              } else {
                (chMap, (chMap(peer), false))
              }
            }
      (cDef, newChannel) = ret
      _                  <- Applicative[Task].whenA(newChannel)(createChannel(peer) >>= cDef.complete)
      c                  <- cDef.get
      // In case underlying gRPC transport is terminated - clean resources,
      // remove current record and try one more time
      r <- if (c.grpcTransport.isTerminated)
            Task.delay(c.buferSubscriber.cancel()) >>
              log.info(
                s"Channel to peer ${peer.toAddress} is terminated, removing from connections map"
              ) >>
              channelsMap.update(_ - peer) >> getChannel(peer)
          else c.pure[Task]
    } yield r

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration)(
      request: GrpcTransport.Request[A]
  ): Task[CommErr[A]] =
    (for {
      channel <- getChannel(peer)
      stub    <- Task.delay(RoutingGrpcMonix.stub(channel.grpcTransport).withDeadlineAfter(timeout))
      result  <- request(stub)
      _       <- Task.unit.asyncBoundary // return control to caller thread
    } yield result).attempt.map(_.fold(e => Left(protocolException(e)), identity))

  def send(peer: PeerNode, msg: Protocol): Task[CommErr[Unit]] =
    withClient(peer, DefaultSendTimeout)(GrpcTransport.send(peer, msg))

  def broadcast(peers: Seq[PeerNode], msg: Protocol): Task[Seq[CommErr[Unit]]] =
    Task.parSequenceUnordered(peers.map(send(_, msg)))

  def stream(peer: PeerNode, blob: Blob): Task[Unit] =
    getChannel(peer).flatMap(_.buffer.enque(blob))

  def stream(peers: Seq[PeerNode], blob: Blob): Task[Unit] =
    Task.parSequenceUnordered(peers.map(stream(_, blob))).void

  private def streamBlobFile(
      path: Path,
      peer: PeerNode,
      sender: PeerNode
  ): Task[Unit] = {

    def timeout(packet: Packet): FiniteDuration =
      Math.max(packet.content.size().toLong * 5, DefaultSendTimeout.toMicros).micros

    PacketOps.restore[Task](path) >>= {
      case Right(packet) =>
        log.debug(
          s"Attempting to stream packet to $peer with timeout: ${timeout(packet).toMillis}ms"
        ) >>
          withClient(peer, timeout(packet))(
            GrpcTransport.stream(networkId, peer, Blob(sender, packet), packetChunkSize)
          ).flatMap {
            case Left(error) =>
              log.debug(
                s"Error while streaming packet to $peer (timeout: ${timeout(packet).toMillis}ms): ${error.message}"
              )
            case Right(_) => log.debug(s"Streamed packet $path to $peer")
          }
      case Left(error) =>
        log.error(s"Error while streaming packet $path to $peer: ${error.message}")
    }
  }
}
