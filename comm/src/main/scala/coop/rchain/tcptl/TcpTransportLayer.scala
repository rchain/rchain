package coop.rchain.tcptl

import scala.concurrent.duration._
import java.net.SocketAddress
import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.p2p.effects._
import coop.rchain.metrics.Metrics
import io.grpc.{Server, ServerBuilder}

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._

import scala.concurrent.{ExecutionContext, Future}

class TcpTransportLayer[F[_]: Monad: Capture: Metrics: Futurable](port: Int)(loc: ProtocolNode)(
    implicit executionContext: ExecutionContext)
    extends TransportLayer[F] {

  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration): F[CommErr[ProtocolMessage]] = ???

  def local: F[ProtocolNode] = loc.pure[F]

  def commSend(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] = ???

  def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] =
    peers.toList.traverse(peer => commSend(msg, peer)).map(_.toSeq)

  def receive(dispatch: ProtocolMessage => F[Option[ProtocolMessage]]): F[Unit] =
    Capture[F].capture {
      ServerBuilder
        .forPort(port)
        .addService(
          TransportLayerGrpc.bindService(new TranportLayerImpl[F](dispatch), executionContext))
        .build
        .start
    }
}

class TranportLayerImpl[F[_]: Monad: Capture: Metrics: Futurable](
    dispatch: ProtocolMessage => F[Option[ProtocolMessage]])
    extends TransportLayerGrpc.TransportLayer {

  def send(request: TLRequest): Future[TLResponse] =
    (request.protocol
      .fold(internalServerError.pure[F]) { protocol =>
        dispatch(toProtocolMessage(protocol)) >>= {
          case None     => noResponse.pure[F]
          case Some(pm) => returnProtocol(pm.proto).pure[F]
        }
      })
      .toFuture

  private def toProtocolMessage(protocol: Protocol): ProtocolMessage = ???

  private def returnProtocol(protocol: Protocol): TLResponse =
    TLResponse(TLResponse.Payload.Protocol(protocol))

  private def internalServerError: TLResponse =
    TLResponse(TLResponse.Payload.InternalServerError(InternalServerError()))

  private def noResponse: TLResponse =
    TLResponse(TLResponse.Payload.NoResponse(NoResponse()))
}
