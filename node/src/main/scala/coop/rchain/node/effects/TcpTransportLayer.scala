package coop.rchain.node.effects

import java.io.File

import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.p2p.effects._
import coop.rchain.metrics.Metrics

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._

import scala.concurrent.duration._
import scala.util.Try
import scala.concurrent.Await
import scala.concurrent.{ExecutionContext, Future}
import io.grpc.netty._
import io.netty.handler.ssl.{ClientAuth, SslContext}
import coop.rchain.comm.protocol.routing.TransportLayerGrpc.TransportLayerStub

// TODO Add State Monad to reuse channels to known peers
class TcpTransportLayer[F[_]: Monad: Capture: Metrics: Futurable](
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

  private def withClient[A](remote: PeerNode)(f: TransportLayerStub => Future[A]): Future[A] = {
    val channel = clientChannel(remote)
    val stub    = TransportLayerGrpc.stub(channel)
    f(stub).andThen { case _ => channel.shutdown() }
  }

  private def clientChannel(remote: PeerNode) =
    NettyChannelBuilder
      .forAddress(remote.endpoint.host, remote.endpoint.tcpPort)
      .negotiationType(NegotiationType.TLS)
      .sslContext(clientSslContext)
      .intercept(new SslSessionClientInterceptor())
      .overrideAuthority(remote.id.toString)
      .build()

  def roundTrip(msg: ProtocolMessage,
                remote: PeerNode,
                timeout: Duration): F[CommErr[ProtocolMessage]] =
    for {
      tlResponseErr <- Capture[F].capture(
                        Try(
                          Await.result(withClient(remote)(_.send(TLRequest(msg.proto.some))),
                                       timeout)
                        ).toEither.leftMap(protocolException))
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

  def send(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] =
    Capture[F]
      .capture(withClient(peer)(_.send(TLRequest(msg.proto.some))))
      .as(Right(()))

  def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] =
    peers.toList.traverse(peer => send(msg, peer)).map(_.toSeq)

  def receive(dispatch: ProtocolMessage => F[CommunicationResponse]): F[Unit] =
    Capture[F].capture {
      NettyServerBuilder
        .forPort(port)
        .sslContext(serverSslContext)
        .addService(
          TransportLayerGrpc.bindService(new TranportLayerImpl[F](dispatch), executionContext))
        .intercept(new SslSessionServerInterceptor())
        .build
        .start
    }
}

class TranportLayerImpl[F[_]: Monad: Capture: Metrics: Futurable](
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
