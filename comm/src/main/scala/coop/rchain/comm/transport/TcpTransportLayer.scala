package coop.rchain.comm.transport

import java.io.ByteArrayInputStream

import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper
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

  private def sendRequest(peer: PeerNode, request: TLRequest, enforce: Boolean): Task[TLResponse] =
    withClient(peer, enforce)(_.send(request))

  private def innerRoundTrip(
      peer: PeerNode,
      request: TLRequest,
      timeout: FiniteDuration,
      enforce: Boolean
  ): Task[Either[CommError, TLResponse]] =
    sendRequest(peer, request, enforce)
      .nonCancelingTimeout(timeout)
      .attempt
      .map(_.leftMap {
        case _: TimeoutException => CommError.timeout
        case e: StatusRuntimeException if e.getStatus.getCode == Status.Code.UNAVAILABLE =>
          peerUnavailable(peer)
        case e => protocolException(e)
      })

  // TODO: Rename to send
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): Task[CommErr[Protocol]] =
    for {
      tlResponseErr <- innerRoundTrip(peer, TLRequest(msg.some), timeout, enforce = false)
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

  // TODO: rename to sendAndForget
  def send(peer: PeerNode, msg: Protocol): Task[Unit] =
    Task
      .racePair(sendRequest(peer, TLRequest(msg.some), enforce = false), Task.unit)
      .attempt
      .void

  def broadcast(peers: Seq[PeerNode], msg: Protocol): Task[Unit] =
    Task.gatherUnordered(peers.map(send(_, msg))).void

  private def buildServer(transportLayer: RoutingGrpcMonix.TransportLayer): Task[Server] =
    Task.delay {
      NettyServerBuilder
        .forPort(port)
        .maxMessageSize(maxMessageSize)
        .sslContext(serverSslContext)
        .addService(RoutingGrpcMonix.bindService(transportLayer, scheduler))
        .intercept(new SslSessionServerInterceptor())
        .build
        .start
    }

  def receive(dispatch: Protocol => Task[CommunicationResponse]): Task[Unit] =
    cell.modify { s =>
      for {
        server <- s.server match {
                   case Some(_) =>
                     Task.raiseError(
                       new RuntimeException("TransportLayer server is already started")
                     )
                   case _ => buildServer(new TransportLayerImpl(dispatch))
                 }
      } yield s.copy(server = Some(server))

    }

  def shutdown(msg: Protocol): Task[Unit] = {
    def shutdownServer: Task[Unit] = cell.modify { s =>
      for {
        _ <- log.info("Shutting down server")
        _ <- s.server.fold(Task.unit)(server => Task.delay(server.shutdown()))
      } yield s.copy(server = None, shutdown = true)
    }

    def sendShutdownMessages: Task[Unit] =
      for {
        peers <- cell.read.map(_.connections.keys.toSeq)
        _     <- log.info("Sending shutdown message to all peers")
        _     <- sendShutdownMessage(peers, msg)
        _     <- log.info("Disconnecting from all peers")
        _     <- Task.gatherUnordered(peers.map(disconnect))
      } yield ()

    def sendShutdownMessage(peers: Seq[PeerNode], msg: Protocol): Task[Unit] = {
      val createInstruction: PeerNode => Task[Unit] =
        innerRoundTrip(_, TLRequest(msg.some), 500.milliseconds, enforce = true).as(())
      Task.gatherUnordered(peers.map(createInstruction)).void
    }

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
    server: Option[Server] = None,
    shutdown: Boolean = false
)

object TransportState {
  def empty: TransportState = TransportState()
}

class TransportLayerImpl(dispatch: Protocol => Task[CommunicationResponse])
    extends RoutingGrpcMonix.TransportLayer {

  def send(request: TLRequest): Task[TLResponse] =
    request.protocol
      .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
        dispatch(protocol) map {
          case NotHandled(error)            => internalServerError(error.message)
          case HandledWitoutMessage         => noResponse
          case HandledWithMessage(response) => returnProtocol(response)
        }
      }

  private def returnProtocol(protocol: Protocol): TLResponse =
    TLResponse(TLResponse.Payload.Protocol(protocol))

  // TODO InternalServerError should take msg in constructor
  private def internalServerError(msg: String): TLResponse =
    TLResponse(
      TLResponse.Payload
        .InternalServerError(InternalServerError(ProtocolHelper.toProtocolBytes(msg)))
    )

  private def noResponse: TLResponse =
    TLResponse(TLResponse.Payload.NoResponse(NoResponse()))
}
