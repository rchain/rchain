package coop.rchain.comm.transport

import java.io.ByteArrayInputStream
import java.nio.file.Path

import scala.concurrent.duration.{FiniteDuration, _}
import scala.util._

import cats.implicits._

import coop.rchain.catscontrib.ski.kp
import coop.rchain.comm._
import coop.rchain.comm.CachedConnections.ConnectionsCache
import coop.rchain.comm.protocol.routing.{Protocol, RoutingGrpcMonix}
import coop.rchain.comm.CommError.{protocolException, CommErr}
import coop.rchain.grpc.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, UncaughtExceptionLogger}

import io.grpc.ManagedChannel
import io.grpc.netty._
import io.netty.handler.ssl.SslContext
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

class GrpcTransportClient(
    cert: String,
    key: String,
    maxMessageSize: Int,
    tempFolder: Path,
    clientQueueSize: Int
)(
    implicit scheduler: Scheduler,
    val log: Log[Task],
    val metrics: Metrics[Task],
    connectionsCache: ConnectionsCache[Task, TcpConnTag]
) extends TransportLayer[Task] {

  val DefaultSendTimeout: FiniteDuration                 = 5.seconds
  val DefaultStreamTimeout: FiniteDuration               = 10.minutes
  private val cache: CachedConnections[Task, TcpConnTag] = connectionsCache(clientChannel)

  implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  private def certInputStream  = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream   = new ByteArrayInputStream(key.getBytes())
  private val streamObservable = new StreamObservable(clientQueueSize, tempFolder)

  // TODO FIX-ME No throwing exceptions!
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private lazy val clientSslContext: SslContext =
    try {
      GrpcSslContexts.forClient
        .trustManager(HostnameTrustManagerFactory.Instance)
        .keyManager(certInputStream, keyInputStream)
        .build
    } catch {
      case e: Throwable =>
        println(e.getMessage)
        throw e
    }

  private def clientChannel(peer: PeerNode): Task[ManagedChannel] =
    for {
      _ <- log.debug(s"Creating new channel to peer ${peer.toAddress}")
      c <- Task.delay {
            NettyChannelBuilder
              .forAddress(peer.endpoint.host, peer.endpoint.tcpPort)
              .executor(scheduler)
              .maxInboundMessageSize(maxMessageSize)
              .negotiationType(NegotiationType.TLS)
              .sslContext(clientSslContext)
              .intercept(new SslSessionClientInterceptor())
              .overrideAuthority(peer.id.toString)
              .build()
          }
    } yield c

  private def createStub(
      peer: PeerNode,
      timeout: FiniteDuration,
      enforce: Boolean
  ): Task[RoutingGrpcMonix.TransportLayer] =
    for {
      channel <- cache.connection(peer, enforce)
      stub    <- Task.delay(RoutingGrpcMonix.stub(channel).withDeadlineAfter(timeout))
    } yield stub

  def disconnect(peer: PeerNode): Task[Unit] =
    cache.modify { s =>
      s.connections
        .get(peer)
        .fold(Task.unit) { c =>
          log.debug(
            s"Disconnecting from peer ${peer.toAddress}"
          ) >> Task.delay(c.shutdown()).attempt.void
        } map kp(s.copy(connections = s.connections - peer))
    }

  private def withClient[A](peer: PeerNode, timeout: FiniteDuration, enforce: Boolean)(
      request: GrpcTransport.Request[A]
  ): Task[CommErr[A]] =
    (for {
      stub   <- createStub(peer, timeout, enforce)
      result <- request(stub)
      _      <- Task.unit.asyncBoundary // return control to caller thread
    } yield result).attempt.flatMap {
      case Right(result @ Left(PeerUnavailable(_))) => disconnect(peer).map(kp(result))
      case Right(result)                            => Task.now(result)
      case Left(e)                                  => Task.now(Left(protocolException(e)))
    }

  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): Task[CommErr[Protocol]] =
    withClient(peer, timeout, enforce = false)(GrpcTransport.roundTrip(peer, msg))

  private def send(
      peer: PeerNode,
      msg: Protocol,
      timeout: FiniteDuration,
      enforce: Boolean
  ): Task[CommErr[Unit]] =
    withClient(peer, timeout, enforce)(GrpcTransport.send(peer, msg))

  def send(peer: PeerNode, msg: Protocol): Task[CommErr[Unit]] =
    send(peer, msg, DefaultSendTimeout, enforce = false)

  def broadcast(peers: Seq[PeerNode], msg: Protocol): Task[Seq[CommErr[Unit]]] =
    Task.gatherUnordered(peers.map(send(_, msg)))

  def stream(peers: Seq[PeerNode], blob: Blob): Task[Unit] =
    streamObservable.stream(peers.toList, blob) >> log.info(s"stream to $peers blob")

  private def streamBlobFile(
      path: Path,
      peer: PeerNode,
      sender: PeerNode,
      messageSize: Int,
      retries: Int = 3,
      delayBetweenRetries: FiniteDuration = 1.second
  ): Task[Unit] = {

    def delay[A](a: => Task[A]): Task[A] =
      Task.defer(a).delayExecution(delayBetweenRetries)

    def handle(retryCount: Int): Task[Unit] =
      if (retryCount > 0)
        PacketOps.restore[Task](path) >>= {
          case Right(packet) =>
            withClient(peer, DefaultStreamTimeout, enforce = false)(
              GrpcTransport.stream(peer, Blob(sender, packet), messageSize)
            ).flatMap {
              case Left(error) =>
                log.error(
                  s"Error while streaming packet to $peer: ${error.message}"
                ) >> delay(handle(retryCount - 1))
              case Right(_) => log.info(s"Streamed packet $path to $peer")
            }
          case Left(error) =>
            log.error(s"Error while streaming packet $path to $peer: ${error.message}")
        } else log.debug(s"Giving up on streaming packet $path to $peer")

    handle(retries)
  }

  private def innerBroadcast(peers: Seq[PeerNode], msg: Protocol): Task[Seq[CommErr[Unit]]] =
    Task.gatherUnordered(peers.map(send(_, msg, 500.milliseconds, enforce = true)))

  def start(): Task[Unit] = {

    def initQueue(maybeQueue: Option[Cancelable])(create: Task[Cancelable]): Task[Cancelable] =
      maybeQueue.fold(create) {
        kp(
          Task.raiseError[Cancelable](
            new RuntimeException("TransportLayer server is already started")
          )
        )
      }

    cache.modify { s =>
      val parallelism = Math.max(Runtime.getRuntime.availableProcessors(), 2)
      val queueScheduler =
        Scheduler.fixedPool("tl-dispatcher-client", parallelism, reporter = UncaughtExceptionLogger)
      for {
        clientQueue <- initQueue(s.clientQueue) {
                        import coop.rchain.shared.PathOps.PathDelete
                        Task.delay {
                          streamObservable
                            .flatMap { s =>
                              Observable
                                .fromIterable(s.peers)
                                .mapParallelUnordered(parallelism)(
                                  streamBlobFile(s.path, _, s.sender, maxMessageSize)
                                )
                                .guarantee(s.path.deleteSingleFile[Task]())
                            }
                            .subscribe()(queueScheduler)
                        }
                      }
      } yield s.copy(server = None, clientQueue = Some(clientQueue))
    }
  }

  def shutdown(msg: Protocol): Task[Unit] = {
    def shutdownQueue: Task[Unit] = cache.modify { s =>
      for {
        _ <- log.info("Shutting down transport layer")
        _ <- s.clientQueue.fold(Task.unit)(q => Task.delay(q.cancel()))
      } yield s.copy(server = None, shutdown = true)
    }

    def sendShutdownMessages: Task[Unit] =
      for {
        peers <- cache.read.map(_.connections.keys.toSeq)
        _     <- log.info("Sending shutdown message to all peers")
        _     <- innerBroadcast(peers, msg)
        _     <- log.info("Disconnecting from all peers")
        _     <- Task.gatherUnordered(peers.map(disconnect))
      } yield ()

    cache.read.flatMap { s =>
      if (s.shutdown) Task.unit
      else shutdownQueue >> sendShutdownMessages
    }
  }

}
