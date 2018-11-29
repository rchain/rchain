package coop.rchain.comm.transport

import java.io.ByteArrayInputStream

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util._

import cats.implicits._

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.CachedConnections.ConnectionsCache
import coop.rchain.comm.CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.protocol.routing.RoutingGrpcMonix.TransportLayerStub
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.shared._
import coop.rchain.shared.Compression._
import java.nio.file._
import io.grpc._
import io.grpc.netty._
import io.netty.handler.ssl._
import monix.eval._
import monix.execution._
import monix.reactive._

class TcpTransportLayer(port: Int, cert: String, key: String, maxMessageSize: Int, tempFolder: Path)(
    implicit scheduler: Scheduler,
    log: Log[Task],
    connectionsCache: ConnectionsCache[Task, TcpConnTag]
) extends TransportLayer[Task] {

  private val DefaultSendTimeout = 5.seconds
  private val connections        = connectionsCache(clientChannel)

  private implicit val logSource: LogSource = LogSource(this.getClass)

  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  private val streamObservable = new StreamObservable(100, tempFolder)

  import connections.cell

  private lazy val serverSslContext: SslContext =
    try {
      GrpcSslContexts
        .configure(SslContextBuilder.forServer(certInputStream, keyInputStream))
        .trustManager(HostnameTrustManagerFactory.Instance)
        .clientAuth(ClientAuth.REQUIRE)
        .build()
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }

  private lazy val clientSslContext: SslContext =
    try {
      val builder = GrpcSslContexts.forClient
      builder.trustManager(HostnameTrustManagerFactory.Instance)
      builder.keyManager(certInputStream, keyInputStream)
      builder.build
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

  def disconnect(peer: PeerNode): Task[Unit] =
    cell.modify { s =>
      for {
        _ <- s.connections.get(peer) match {
              case Some(c) =>
                log
                  .debug(s"Disconnecting from peer ${peer.toAddress}")
                  .map(kp(Try(c.shutdown())))
                  .void
              case _ => Task.unit // ignore if connection does not exists already
            }
      } yield s.copy(connections = s.connections - peer)
    }

  private object PeerUnavailable {
    def unapply(e: Throwable): Boolean =
      e.isInstanceOf[StatusRuntimeException] &&
        e.asInstanceOf[StatusRuntimeException].getStatus.getCode == Status.Code.UNAVAILABLE
  }

  private def withClient[A](peer: PeerNode, enforce: Boolean)(
      f: TransportLayerStub => Task[A]
  ): Task[A] =
    for {
      channel <- connections.connection(peer, enforce)
      stub    <- Task.delay(RoutingGrpcMonix.stub(channel))
      result <- f(stub).doOnFinish {
                 case Some(PeerUnavailable()) => disconnect(peer)
                 case _                       => Task.unit
               }
      _ <- Task.unit.asyncBoundary // return control to caller thread
    } yield result

  private def transport(peer: PeerNode, enforce: Boolean)(
      f: TransportLayerStub => Task[TLResponse]
  ): Task[CommErr[Option[Protocol]]] =
    withClient(peer, enforce)(f).attempt.map(processResponse(peer, _))

  def chunkIt(blob: Blob): Task[Iterator[Chunk]] =
    Task.delay {
      val raw      = blob.packet.content.toByteArray
      val kb500    = 1024 * 500
      val compress = raw.length > kb500
      val content  = if (compress) raw.compress else raw

      def header: Chunk =
        Chunk().withHeader(
          ChunkHeader()
            .withCompressed(compress)
            .withContentLength(raw.length)
            .withSender(ProtocolHelper.node(blob.sender))
            .withTypeId(blob.packet.typeId)
        )
      val buffer    = 2 * 1024 // 2 kbytes for protobuf related stuff
      val chunkSize = maxMessageSize - buffer
      def data: Iterator[Chunk] =
        content.sliding(chunkSize, chunkSize).map { data =>
          Chunk().withData(ChunkData().withContentData(ProtocolHelper.toProtocolBytes(data)))
        }

      Iterator(header) ++ data
    }

  def stream(peers: Seq[PeerNode], blob: Blob): Task[Unit] =
    streamObservable.stream(peers.toList, blob) *> log.info(s"stream to $peers blob")

  private def processResponse(
      peer: PeerNode,
      response: Either[Throwable, TLResponse]
  ): CommErr[Option[Protocol]] =
    response
      .leftMap {
        case _: TimeoutException => CommError.timeout
        case PeerUnavailable()   => peerUnavailable(peer)
        case e                   => protocolException(e)
      }
      .flatMap(
        tlr =>
          tlr.payload match {
            case p if p.isProtocol   => Right(Some(tlr.getProtocol))
            case p if p.isNoResponse => Right(None)
            case TLResponse.Payload.InternalServerError(ise) =>
              Left(internalCommunicationError("Got response: " + ise.error.toStringUtf8))
          }
      )

  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): Task[CommErr[Protocol]] =
    transport(peer, enforce = false)(_.ask(TLRequest(msg.some)).nonCancelingTimeout(timeout))
      .map(_.flatMap {
        case Some(p) => Right(p)
        case _       => Left(internalCommunicationError("Was expecting message, nothing arrived"))
      })

  private def innerSend(
      peer: PeerNode,
      msg: Protocol,
      enforce: Boolean = false,
      timeout: FiniteDuration = DefaultSendTimeout
  ): Task[CommErr[Unit]] =
    transport(peer, enforce)(_.ask(TLRequest(msg.some)).nonCancelingTimeout(timeout))
      .map(_.flatMap {
        case Some(p) => Left(internalCommunicationError(s"Was expecting no message. Response: $p"))
        case _       => Right(())
      })

  private def innerBroadcast(
      peers: Seq[PeerNode],
      msg: Protocol,
      enforce: Boolean = false,
      timeOut: FiniteDuration = DefaultSendTimeout
  ): Task[Seq[CommErr[Unit]]] =
    Task.gatherUnordered(peers.map(innerSend(_, msg, enforce, timeOut)))

  def send(peer: PeerNode, msg: Protocol): Task[CommErr[Unit]] =
    innerSend(peer, msg)

  def broadcast(peers: Seq[PeerNode], msg: Protocol): Task[Seq[CommErr[Unit]]] =
    innerBroadcast(peers, msg)

  def handleToStream: ToStream => Task[Unit] = {
    case ToStream(peer, path, sender) =>
      PacketOps.restore[Task](path) >>= {
        case Right(packet) =>
          withClient(peer, enforce = false) { stub =>
            val blob = Blob(sender, packet)
            stub.stream(Observable.fromIterator(chunkIt(blob)))
          }.attempt
            .flatMap {
              case Left(error) => log.error(s"Error while streaming packet, error: $error")
              case Right(_)    => Task.unit
            }
        case Left(error) => log.error(s"Error while streaming packet, error: $error")
      } >>=
        (kp(Task.delay {
          if (path.toFile.exists)
            path.toFile.delete
        }))
  }

  private def initQueue(
      maybeQueue: Option[Cancelable]
  )(create: Task[Cancelable]): Task[Cancelable] =
    maybeQueue.fold(create) {
      kp(
        Task.raiseError[Cancelable](
          new RuntimeException("TransportLayer server is already started")
        )
      )
    }

  def receive(
      dispatch: Protocol => Task[CommunicationResponse],
      handleStreamed: Blob => Task[Unit]
  ): Task[Unit] = {
    val dispatchInternal: ServerMessage => Task[Unit] = {
      // TODO: consider logging on failure (Left)
      case Tell(protocol) => dispatch(protocol).attemptAndLog.void
      case Ask(protocol, handle) if !handle.complete =>
        dispatch(protocol).attempt.map {
          case Left(e)         => handle.failWith(e)
          case Right(response) => handle.reply(response)
        }.void
      case StreamMessage(blob) => handleStreamed(blob).attemptAndLog
      case _                   => Task.unit // sender timeout
    }

    cell.modify { s =>
      val parallelism = Math.max(Runtime.getRuntime.availableProcessors(), 2)
      val queueScheduler =
        Scheduler.fixedPool("tl-dispatcher", parallelism, reporter = UncaughtExceptionLogger)
      for {
        server <- initQueue(s.server) {
                   Task.delay {
                     new TcpServerObservable(port, serverSslContext, maxMessageSize)
                       .mapParallelUnordered(parallelism)(dispatchInternal)
                       .subscribe()(queueScheduler)
                   }

                 }
        clientQueue <- initQueue(s.clientQueue) {
                        Task.delay {
                          streamObservable
                            .mapParallelUnordered(parallelism)(handleToStream)
                            .subscribe()(queueScheduler)
                        }
                      }
      } yield s.copy(server = Some(server), clientQueue = Some(clientQueue))
    }

  }

  def shutdown(msg: Protocol): Task[Unit] = {
    def shutdownServer: Task[Unit] = cell.modify { s =>
      for {
        _ <- log.info("Shutting down server")
        _ <- s.server.fold(Task.unit)(server => Task.delay(server.cancel()))
        _ <- s.clientQueue.fold(Task.unit)(server => Task.delay(server.cancel()))
      } yield s.copy(server = None, shutdown = true)
    }

    def sendShutdownMessages: Task[Unit] =
      for {
        peers <- cell.read.map(_.connections.keys.toSeq)
        _     <- log.info("Sending shutdown message to all peers")
        _     <- innerBroadcast(peers, msg, enforce = true, timeOut = 500.milliseconds)
        _     <- log.info("Disconnecting from all peers")
        _     <- Task.gatherUnordered(peers.map(disconnect))
      } yield ()

    cell.read.flatMap { s =>
      if (s.shutdown) Task.unit
      else shutdownServer *> sendShutdownMessages
    }
  }
}

object TcpTransportLayer {
  type Connection          = ManagedChannel
  type Connections         = Map[PeerNode, Connection]
  type TransportCell[F[_]] = Cell[F, TransportState]
}

case class TransportState(
    connections: TcpTransportLayer.Connections = Map.empty,
    server: Option[Cancelable] = None,
    clientQueue: Option[Cancelable] = None,
    shutdown: Boolean = false
)

object TransportState {
  def empty: TransportState = TransportState()
}
