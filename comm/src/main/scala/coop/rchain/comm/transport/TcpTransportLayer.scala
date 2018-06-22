package coop.rchain.comm.transport

import java.io.File

import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.metrics.Metrics

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._

import scala.concurrent.duration._
import scala.util._
import scala.concurrent.Await
import scala.concurrent.{ExecutionContext, Future}
import io.grpc._, io.grpc.netty._
import io.netty.handler.ssl.{ClientAuth, SslContext}
import coop.rchain.comm.protocol.routing.TransportLayerGrpc.TransportLayerStub
import coop.rchain.comm.transport.TcpTransportLayer._

class TcpTransportLayer[
    F[_]: Monad: Capture: Metrics: Futurable: TcpTransportLayer.ConnectionsState](
    host: String,
    port: Int,
    cert: File,
    key: File)(src: PeerNode)(implicit executionContext: ExecutionContext)
    extends TransportLayer[F] {

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

  private def clientChannel(remote: PeerNode): ManagedChannel =
    NettyChannelBuilder
      .forAddress(remote.endpoint.host, remote.endpoint.tcpPort)
      .negotiationType(NegotiationType.TLS)
      .sslContext(clientSslContext)
      .intercept(new SslSessionClientInterceptor())
      .overrideAuthority(remote.id.toString)
      .build()

  private def connection(remote: PeerNode): F[TransportLayerStub] =
    for {
      connections <- ConnectionsState[F].get
      stub        = connections.getOrElse(remote.id, TransportLayerGrpc.stub(clientChannel(remote)))
      _           <- ConnectionsState[F].modify(_ + (remote.id -> stub))
    } yield stub

  def disconnect(remote: PeerNode): F[Unit] =
    for {
      connections <- ConnectionsState[F].get
      _ = connections
        .get(remote.id)
        .foreach(c => Try(c.getChannel.asInstanceOf[ManagedChannel].shutdown()))
      _ <- ConnectionsState[F].modify(_ - remote.id)
    } yield ()

  private def sendRequest(msg: ProtocolMessage,
                          remote: PeerNode,
                          timeout: Duration): F[Either[CommError, TLResponse]] =
    for {
      stub   <- connection(remote)
      result <- Capture[F].capture(Try(Await.result(stub.send(TLRequest(msg.proto.some)), timeout)))
      resp <- result match {
               case Success(response) =>
                 Either.right[ProtocolException, TLResponse](response).pure[F]
               case Failure(e) =>
                 disconnect(remote).map(_ =>
                   Either.left[CommError, TLResponse](protocolException(e)))
             }
    } yield resp

  def roundTrip(msg: ProtocolMessage,
                remote: PeerNode,
                timeout: Duration): F[CommErr[ProtocolMessage]] =
    for {
      tlResponseErr <- sendRequest(msg, remote, timeout)
      pmErr <- tlResponseErr
                .flatMap(tlr =>
                  tlr.payload match {
                    case p if p.isProtocol => ProtocolMessage.toProtocolMessage(tlr.getProtocol)
                    case p if p.isNoResponse =>
                      Left(internalCommunicationError("Was expecting message, nothing arrived"))
                    case p if p.isInternalServerError =>
                      Left(internalCommunicationError("crap"))
                })
                .pure[F]
    } yield pmErr

  val local: F[PeerNode] = src.pure[F]

  // TODO: Perform a disconnect on failure
  def send(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] =
    for {
      stub <- connection(peer)
      res  <- Capture[F].capture(stub.send(TLRequest(msg.proto.some))).as(Right(()))
    } yield res

  def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] =
    peers.toList.traverse(peer => send(msg, peer)).map(_.toSeq)

  def receive(dispatch: ProtocolMessage => F[CommunicationResponse]): F[Unit] =
    Capture[F].capture {
      NettyServerBuilder
        .forPort(port)
        .sslContext(serverSslContext)
        .addService(
          TransportLayerGrpc.bindService(new TransportLayerImpl[F](dispatch), executionContext))
        .intercept(new SslSessionServerInterceptor())
        .build
        .start
    }
}

object TcpTransportLayer {
  import cats.mtl.MonadState
  type Connection             = TransportLayerStub
  type Connections            = Map[NodeIdentifier, Connection]
  type ConnectionsState[F[_]] = MonadState[F, Connections]

  object ConnectionsState {
    def apply[F[_]](implicit CS: ConnectionsState[F]): ConnectionsState[F] = CS
  }
}

class TransportLayerImpl[F[_]: Monad: Capture: Metrics: Futurable](
    dispatch: ProtocolMessage => F[CommunicationResponse])
    extends TransportLayerGrpc.TransportLayer {

  def send(request: TLRequest): Future[TLResponse] =
    request.protocol
      .fold(internalServerError("protocol not available in request").pure[F]) { protocol =>
        ProtocolMessage.toProtocolMessage(protocol) match {
          case Left(error) => internalServerError(error.toString).pure[F]
          case Right(pm) =>
            dispatch(pm) >>= {
              case NotHandled                   => internalServerError(s"Message $pm was not handled!").pure[F]
              case HandledWitoutMessage         => noResponse.pure[F]
              case HandledWithMessage(response) => returnProtocol(response.proto).pure[F]
            }
        }
      }
      .toFuture

  private def returnProtocol(protocol: Protocol): TLResponse =
    TLResponse(TLResponse.Payload.Protocol(protocol))

  // TODO InternalServerError should take msg in constructor
  private def internalServerError(msg: String): TLResponse =
    TLResponse(TLResponse.Payload.InternalServerError(InternalServerError()))

  private def noResponse: TLResponse =
    TLResponse(TLResponse.Payload.NoResponse(NoResponse()))
}
