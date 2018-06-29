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
import coop.rchain.comm.transport.TcpTransportLayer._
import monix.eval._, monix.execution._
import scala.concurrent.TimeoutException

class TcpTransportLayer(host: String, port: Int, cert: File, key: File)(src: PeerNode)(
    implicit scheduler: Scheduler,
    connections: ConnectionsState,
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

  private def connection(peer: PeerNode): Task[ManagedChannel] =
    for {
      cs <- connections.get
      c  <- cs.get(peer.id).fold(clientChannel(peer))(_.pure[Task])
      _  <- connections.modify(_ + (peer.id -> c))
    } yield c

  def disconnect(peer: PeerNode): Task[Unit] =
    for {
      cs <- connections.get
      _ <- cs.get(peer.id) match {
            case Some(c) => Task.delay(c.shutdown()).attempt.void
            case _       => log.warn(s"Can't disconnect from peer ${peer.id}. Connection not found.")
          }
      _ <- connections.modify(_ - peer.id)
    } yield ()

  private def withClient[A](peer: PeerNode)(f: TransportLayerStub => Task[A]): Task[A] =
    for {
      channel <- connection(peer)
      stub    <- Task.delay(TransportLayerGrpc.stub(channel))
      result <- f(stub).doOnFinish {
                 case None    => Task.unit
                 case Some(_) => disconnect(peer)
               }
    } yield result

  private def innerSend(peer: PeerNode, request: TLRequest): Task[TLResponse] =
    withClient(peer)(stub => Task.fromFuture(stub.send(request)))
      .doOnFinish {
        case None    => Task.unit
        case Some(e) => log.warn(s"Failed to send a message to peer ${peer.id}: ${e.getMessage}")
      }

  private def sendRequest(request: TLRequest,
                          peer: PeerNode,
                          timeout: FiniteDuration): Task[Either[CommError, TLResponse]] =
    innerSend(peer, request)
      .nonCancelingTimeout(timeout)
      .attempt
      .map(_.leftMap {
        case _: TimeoutException => TimeOut
        case e                   => protocolException(e)
      })

  // TODO: Rename to send
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): Task[CommErr[Protocol]] =
    for {
      tlResponseErr <- sendRequest(TLRequest(msg.some), peer, timeout)
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
      .racePair(innerSend(peer, TLRequest(msg.some)), Task.unit)
      .attempt
      .void

  def broadcast(peers: Seq[PeerNode], msg: Protocol): Task[Unit] =
    Task.gatherUnordered(peers.map(send(_, msg))).void

  def receive(dispatch: Protocol => Task[CommunicationResponse]): Task[Unit] =
    Capture[Task].capture {
      NettyServerBuilder
        .forPort(port)
        .sslContext(serverSslContext)
        .addService(TransportLayerGrpc.bindService(new TransportLayerImpl(dispatch), scheduler))
        .intercept(new SslSessionServerInterceptor())
        .build
        .start
    }
}

object TcpTransportLayer {
  import cats.mtl.MonadState
  type Connection       = ManagedChannel
  type Connections      = Map[NodeIdentifier, Connection]
  type ConnectionsState = MonadState[Task, Connections]
}

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
