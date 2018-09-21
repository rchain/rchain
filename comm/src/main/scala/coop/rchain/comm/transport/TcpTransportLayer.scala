package coop.rchain.comm.transport

import java.io.ByteArrayInputStream

import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import coop.rchain.shared.{Cell, Log, LogSource}

import scala.concurrent.duration._
import scala.util._
import io.grpc._, io.grpc.netty._
import io.netty.handler.ssl.{ClientAuth, SslContext, SslContextBuilder}
import coop.rchain.comm.protocol.routing.RoutingGrpcMonix.TransportLayerStub
import monix.eval._, monix.execution._
import scala.concurrent.TimeoutException

class TcpTransportLayer(host: String, port: Int, cert: String, key: String, maxMessageSize: Int)(
    implicit scheduler: Scheduler,
    cell: TcpTransportLayer.TransportCell[Task],
    log: Log[Task]
) extends TransportLayer[Task] {

  private val DefaultSendTimeout = 5.seconds

  private implicit val logSource: LogSource = LogSource(this.getClass)

  private def certInputStream = new ByteArrayInputStream(cert.getBytes())
  private def keyInputStream  = new ByteArrayInputStream(key.getBytes())

  private lazy val serverSslContext: SslContext =
    try {
      GrpcSslContexts
        .configure(SslContextBuilder.forServer(certInputStream, keyInputStream))
        .trustManager(HostnameTrustManagerFactory.Instance)
        .clientAuth(ClientAuth.REQUIRE)
        .build()
    } catch {
      case e: Throwable =>
        println(e.getMessage)
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
              .maxInboundMessageSize(maxMessageSize)
              .negotiationType(NegotiationType.TLS)
              .sslContext(clientSslContext)
              .intercept(new SslSessionClientInterceptor())
              .overrideAuthority(peer.id.toString)
              .build()
          }
    } yield c

  private def connection(peer: PeerNode, enforce: Boolean): Task[ManagedChannel] =
    cell.modify { s =>
      if (s.shutdown && !enforce)
        Task.raiseError(new RuntimeException("The transport layer has been shut down")).as(s)
      else
        for {
          c <- s.connections.get(peer).fold(clientChannel(peer))(_.pure[Task])
        } yield s.copy(connections = s.connections + (peer -> c))
    } >>= kp(cell.read.map(_.connections.apply(peer)))

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

  private def withClient[A](peer: PeerNode, enforce: Boolean)(
      f: TransportLayerStub => Task[A]
  ): Task[A] =
    for {
      channel <- connection(peer, enforce)
      stub    <- Task.delay(RoutingGrpcMonix.stub(channel))
      result <- f(stub).doOnFinish {
                 case None    => Task.unit
                 case Some(_) => disconnect(peer)
               }
    } yield result

  private def innerRoundTrip(
      peer: PeerNode,
      request: TLRequest,
      timeout: FiniteDuration
  ): Task[Either[CommError, TLResponse]] =
    withClient(peer, enforce = false)(_.ask(request).nonCancelingTimeout(timeout)).attempt
      .map(_.leftMap {
        case _: TimeoutException => CommError.timeout
        case e: StatusRuntimeException if e.getStatus.getCode == Status.Code.UNAVAILABLE =>
          peerUnavailable(peer)
        case e => protocolException(e)
      })

  // TODO: Rename to send
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): Task[CommErr[Protocol]] =
    for {
      tlResponseErr <- innerRoundTrip(peer, TLRequest(msg.some), timeout)
      pmErr <- tlResponseErr
                .flatMap(
                  tlr =>
                    tlr.payload match {
                      case p if p.isProtocol => Right(tlr.getProtocol)
                      case p if p.isNoResponse =>
                        Left(internalCommunicationError("Was expecting message, nothing arrived"))
                      case TLResponse.Payload.InternalServerError(ise) =>
                        Left(internalCommunicationError("Got response: " + ise.error.toStringUtf8))
                    }
                )
                .pure[Task]
    } yield pmErr

  private def innerSend(
      peer: PeerNode,
      msg: Protocol,
      enforce: Boolean = false,
      timeOut: FiniteDuration = DefaultSendTimeout
  ): Task[Unit] =
    withClient(peer, enforce) {
      _.tell(TLRequest(msg.some)).nonCancelingTimeoutTo(timeOut, Task.delay(peerUnavailable(peer)))
    }.attempt.void

  private def innerBroadcast(
      peers: Seq[PeerNode],
      msg: Protocol,
      enforce: Boolean = false,
      timeOut: FiniteDuration = DefaultSendTimeout
  ): Task[Unit] =
    Task.gatherUnordered(peers.map(innerSend(_, msg, enforce, timeOut))).void

  def send(peer: PeerNode, msg: Protocol): Task[Unit] =
    innerSend(peer, msg)

  def broadcast(peers: Seq[PeerNode], msg: Protocol): Task[Unit] =
    innerBroadcast(peers, msg)

  private def receiveInternal(
      parallelism: Int
  )(dispatch: Protocol => Task[CommunicationResponse]): Task[Cancelable] = {

    def dispatchInternal: ServerMessage => Task[Unit] = {
      // TODO: consider logging on failure (Left)
      case Tell(protocol) => dispatch(protocol).attempt.void
      case Ask(protocol, sender) =>
        dispatch(protocol).attempt.map {
          case Left(e)         => sender.failWith(e)
          case Right(response) => sender.reply(response)
        }.void
    }

    Task.delay {
      new TcpServerObservable(port, serverSslContext, maxMessageSize)
        .mapParallelUnordered(parallelism)(dispatchInternal)
        .subscribe()
    }
  }

  def receive(dispatch: Protocol => Task[CommunicationResponse]): Task[Unit] =
    cell.modify { s =>
      for {
        server <- s.server match {
                   case Some(_) =>
                     Task.raiseError(
                       new RuntimeException("TransportLayer server is already started")
                     )
                   case _ =>
                     val parallelism = Runtime.getRuntime.availableProcessors()
                     receiveInternal(parallelism)(dispatch)
                 }
      } yield s.copy(server = Some(server))

    }

  def shutdown(msg: Protocol): Task[Unit] = {
    def shutdownServer: Task[Unit] = cell.modify { s =>
      for {
        _ <- log.info("Shutting down server")
        _ <- s.server.fold(Task.unit)(server => Task.delay(server.cancel()))
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
    shutdown: Boolean = false
)

object TransportState {
  def empty: TransportState = TransportState()
}
