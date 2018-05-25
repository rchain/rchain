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
import io.netty.handler.ssl.util.InsecureTrustManagerFactory

class TcpTransportLayer[F[_]: Monad: Capture: Metrics: Futurable](host: String, port: Int)(
    src: PeerNode)(implicit executionContext: ExecutionContext)
    extends TransportLayer[F] {

  private def client(endpoint: Endpoint) = {
    val channel = NettyChannelBuilder
      .forAddress(host, port)
      .negotiationType(NegotiationType.TLS)
      .sslContext(buildClientSslContext("", ""))
      .intercept(new SslSessionClientInterceptor())
      .build()

    TransportLayerGrpc.stub(channel)
  }

  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration): F[CommErr[ProtocolMessage]] =
    for {
      tlResponseErr <- Capture[F].capture {
                        Try(
                          Await.result(client(remote.endpoint).send(TLRequest(msg.proto.some)),
                                       timeout)).toEither
                          .leftMap(protocolException)
                      }
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

  val local: F[ProtocolNode] = ProtocolNode(src).pure[F]

  def send(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] =
    Capture[F]
      .capture(client(peer.endpoint).send(TLRequest(msg.proto.some)))
      .as(Right(()))

  def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] =
    peers.toList.traverse(peer => send(msg, peer)).map(_.toSeq)

  def receive(dispatch: ProtocolMessage => F[Option[ProtocolMessage]]): F[Unit] =
    Capture[F].capture {
      NettyServerBuilder
        .forPort(port)
        .sslContext(buildServerSslContext("", ""))
        .addService(
          TransportLayerGrpc.bindService(new TranportLayerImpl[F](dispatch), executionContext))
        .intercept(new SslSessionServerInterceptor())
        .build
        .start
    }

  def buildServerSslContext(certChainFile: String, privateKeyFile: String): SslContext =
    GrpcSslContexts
      .forServer(new File(certChainFile), new File(privateKeyFile))
      .trustManager(InsecureTrustManagerFactory.INSTANCE)
      .clientAuth(ClientAuth.OPTIONAL)
      .build()

  def buildClientSslContext(certChainFile: String, privateKeyFile: String): SslContext = {
    val builder = GrpcSslContexts.forClient
    builder.trustManager(InsecureTrustManagerFactory.INSTANCE)
    builder.keyManager(new File(certChainFile), new File(privateKeyFile))
    builder.build
  }
}

class TranportLayerImpl[F[_]: Monad: Capture: Metrics: Futurable](
    dispatch: ProtocolMessage => F[Option[ProtocolMessage]])
    extends TransportLayerGrpc.TransportLayer {

  def send(request: TLRequest): Future[TLResponse] =
    request.protocol
      .fold(internalServerError("protocol not available in request").pure[F]) { protocol =>
        ProtocolMessage.toProtocolMessage(protocol) match {
          case Left(error) => internalServerError(error.toString).pure[F]
          case Right(pm) =>
            dispatch(pm) >>= {
              case None     => noResponse.pure[F]
              case Some(pm) => returnProtocol(pm.proto).pure[F]
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
