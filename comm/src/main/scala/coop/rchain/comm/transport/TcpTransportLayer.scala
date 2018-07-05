package coop.rchain.comm.transport

import java.io.File

import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import coop.rchain.shared.{Log, LogSource}

import scala.concurrent.duration._
import scala.util._
import scala.concurrent.Future
import io.grpc._, io.grpc.netty._
import io.netty.handler.ssl.{ClientAuth, SslContext}
import coop.rchain.comm.protocol.routing.TransportLayerGrpc.TransportLayerStub
import monix.eval._, monix.execution._
import scala.concurrent.TimeoutException

class TcpTransportLayer(host: String, port: Int, cert: File, key: File)(src: PeerNode)(
    implicit scheduler: Scheduler,
    state: TcpTransportLayer.State,
    log: Log[Task])
    extends TransportLayer[Task] {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  val local: Task[PeerNode] = src.pure[Task]

  private lazy val serverSslContext: SslContext =
    try {
      GrpcSslContexts
        .forServer(cert, key)
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
      builder.keyManager(cert, key)
      builder.build
    } catch {
      case e: Throwable =>
        println(e.getMessage)
        throw e
    }

  private def clientChannel(peer: PeerNode): Task[ManagedChannel] =
    Task.delay {
      NettyChannelBuilder
        .forAddress(peer.endpoint.host, peer.endpoint.tcpPort)
        .negotiationType(NegotiationType.TLS)
        .sslContext(clientSslContext)
        .intercept(new SslSessionClientInterceptor())
        .overrideAuthority(peer.id.toString)
        .build()
    }

  private def connection(peer: PeerNode, enforce: Boolean): Task[ManagedChannel] =
    for {
      s <- state.get
      _ <- if (s.shutdown && !enforce)
            Task.raiseError(new RuntimeException("The transport layer has been shut down"))
          else Task.unit
      c    <- s.connections.get(peer).fold(clientChannel(peer))(_.pure[Task])
      _    <- state.modify(s => s.copy(connections = s.connections + (peer -> c)))
      cons <- state.inspect(_.connections.keys)
      _    <- log.info(s"Current open connections: ${cons.size}")
    } yield c

  def disconnect(peer: PeerNode): Task[Unit] =
    for {
      s <- state.get
      _ <- s.connections.get(peer) match {
            case Some(c) => Task.delay(c.shutdown()).attempt.void
            case _       => log.warn(s"Can't disconnect from peer ${peer.id}. Connection not found.")
          }
      _    <- state.modify(s => s.copy(connections = s.connections - peer))
      cons <- state.inspect(_.connections.keys)
      _    <- log.info(s"Current open connections: ${cons.size}")
    } yield ()

  private def withClient[A](peer: PeerNode, enforce: Boolean)(
      f: TransportLayerStub => Task[A]): Task[A] =
    for {
      channel <- connection(peer, enforce)
      stub    <- Task.delay(TransportLayerGrpc.stub(channel))
      result <- f(stub).doOnFinish {
                 case None    => Task.unit
                 case Some(_) => disconnect(peer)
               }
    } yield result

  private def sendRequest(peer: PeerNode, request: TLRequest, enforce: Boolean): Task[TLResponse] =
    withClient(peer, enforce)(stub => Task.fromFuture(stub.send(request)))
      .doOnFinish {
        case None    => Task.unit
        case Some(e) => log.warn(s"Failed to send a message to peer ${peer.id}: ${e.getMessage}")
      }

  private def innerRoundTrip(peer: PeerNode,
                             request: TLRequest,
                             timeout: FiniteDuration,
                             enforce: Boolean): Task[Either[CommError, TLResponse]] =
    sendRequest(peer, request, enforce)
      .nonCancelingTimeout(timeout)
      .attempt
      .map(_.leftMap {
        case _: TimeoutException => TimeOut
        case e                   => protocolException(e)
      })

  // TODO: Rename to send
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): Task[CommErr[Protocol]] =
    for {
      tlResponseErr <- innerRoundTrip(peer, TLRequest(msg.some), timeout, enforce = false)
      pmErr <- tlResponseErr
                .flatMap(tlr =>
                  tlr.payload match {
                    case p if p.isProtocol => Right(tlr.getProtocol)
                    case p if p.isNoResponse =>
                      Left(internalCommunicationError("Was expecting message, nothing arrived"))
                    case TLResponse.Payload.InternalServerError(ise) =>
                      Left(internalCommunicationError(ise.error.toStringUtf8))
                })
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

  private def buildServer(transportLayer: TransportLayerGrpc.TransportLayer): Task[Server] =
    Task.delay {
      NettyServerBuilder
        .forPort(port)
        .sslContext(serverSslContext)
        .addService(TransportLayerGrpc.bindService(transportLayer, scheduler))
        .intercept(new SslSessionServerInterceptor())
        .build
        .start
    }

  def receive(dispatch: Protocol => Task[CommunicationResponse]): Task[Unit] =
    for {
      s <- state.get
      server <- s.server match {
                 case Some(_) =>
                   Task.raiseError(new RuntimeException("TransportLayer server is already started"))
                 case _ => buildServer(new TransportLayerImpl(dispatch))
               }
      _ <- state.modify(_.copy(server = Some(server)))
    } yield ()

  def shutdown(msg: Protocol): Task[Unit] =
    for {
      s     <- state.get
      _     <- log.info("Shutting down server")
      _     <- s.server.fold(Task.unit)(server => Task.delay(server.shutdown()))
      _     <- state.modify(_.copy(server = None, shutdown = true))
      peers = s.connections.keys.toSeq
      _     <- log.info("Sending shutdown message to all peers")
      _     <- sendShutdownMessage(peers, msg)
      _     <- log.info("Disconnecting from all peers")
      _     <- Task.gatherUnordered(peers.map(disconnect))
    } yield ()

  private def sendShutdownMessage(peers: Seq[PeerNode], msg: Protocol): Task[Unit] =
    Task
      .gatherUnordered(
        peers.map(innerRoundTrip(_, TLRequest(msg.some), 500.milliseconds, enforce = true))
      )
      .void

}

object TcpTransportLayer {
  import cats.mtl.MonadState
  type Connection  = ManagedChannel
  type Connections = Map[PeerNode, Connection]
  type State       = MonadState[Task, TransportState]
}

case class TransportState(
    connections: TcpTransportLayer.Connections = Map.empty,
    server: Option[Server] = None,
    shutdown: Boolean = false
)

class TransportLayerImpl(dispatch: Protocol => Task[CommunicationResponse])(
    implicit scheduler: Scheduler)
    extends TransportLayerGrpc.TransportLayer {

  def send(request: TLRequest): Future[TLResponse] =
    request.protocol
      .fold(internalServerError("protocol not available in request").pure[Task]) { protocol =>
        dispatch(protocol) map {
          case NotHandled(error)            => internalServerError(s"$error")
          case HandledWitoutMessage         => noResponse
          case HandledWithMessage(response) => returnProtocol(response)
        }
      }
      .runAsync

  private def returnProtocol(protocol: Protocol): TLResponse =
    TLResponse(TLResponse.Payload.Protocol(protocol))

  // TODO InternalServerError should take msg in constructor
  private def internalServerError(msg: String): TLResponse =
    TLResponse(
      TLResponse.Payload.InternalServerError(
        InternalServerError(ProtocolHelper.toProtocolBytes(msg))))

  private def noResponse: TLResponse =
    TLResponse(TLResponse.Payload.NoResponse(NoResponse()))
}
