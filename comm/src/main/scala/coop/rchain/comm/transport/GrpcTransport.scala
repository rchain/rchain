package coop.rchain.comm.transport

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.{RoutingGrpcMonix, _}
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits._
import coop.rchain.monix.Monixable
import coop.rchain.shared.syntax._
import io.grpc.{Status, StatusRuntimeException}
import monix.reactive.Observable

import scala.util.{Either, Left, Right}

object GrpcTransport {

  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  private object PeerUnavailable {
    def unapply(e: Throwable): Boolean =
      e match {
        case sre: StatusRuntimeException =>
          sre.getStatus.getCode == Status.Code.UNAVAILABLE
        case _ => false
      }
  }

  private object PeerTimeout {
    def unapply(e: Throwable): Boolean =
      e match {
        case sre: StatusRuntimeException =>
          sre.getStatus.getCode == Status.Code.DEADLINE_EXCEEDED
        case _ => false
      }
  }

  private object PeerWrongNetwork {
    def unapply(e: Throwable): Option[String] =
      e match {
        case sre: StatusRuntimeException =>
          if (sre.getStatus.getCode == Status.Code.PERMISSION_DENIED)
            Some(sre.getStatus.getDescription)
          else None
        case _ => None
      }
  }

  private object PeerMessageToLarge {
    def unapply(e: Throwable): Boolean =
      e match {
        case sre: StatusRuntimeException =>
          sre.getStatus.getCode == Status.Code.RESOURCE_EXHAUSTED
        case _ => false
      }
  }

  private def processResponse(
      peer: PeerNode,
      response: Either[Throwable, TLResponse]
  ): CommErr[Unit] =
    processError(peer, response)
      .flatMap(
        tlr =>
          tlr.payload match {
            case p if p.isAck => Right(())
            case TLResponse.Payload.InternalServerError(ise) =>
              Left(internalCommunicationError("Got response: " + ise.error.toStringUtf8))
          }
      )

  private def processError[R](
      peer: PeerNode,
      response: Either[Throwable, R]
  ): CommErr[R] =
    response
      .leftMap {
        case PeerTimeout()         => CommError.timeout
        case PeerUnavailable()     => peerUnavailable(peer)
        case PeerMessageToLarge()  => messageToLarge(peer)
        case PeerWrongNetwork(msg) => wrongNetwork(peer, msg)
        case e                     => protocolException(e)
      }

  def send[F[_]: Monixable: Sync](
      transport: RoutingGrpcMonix.TransportLayer,
      peer: PeerNode,
      msg: Protocol
  )(
      implicit metrics: Metrics[F]
  ): F[CommErr[Unit]] =
    for {
      _ <- metrics.incrementCounter("send")
      result <- transport
                 .send(TLRequest(msg.some))
                 .fromTask
                 .attempt
                 .timer("send-time")
                 .map(processResponse(peer, _))
    } yield result

  def stream[F[_]: Monixable: Sync](
      transport: RoutingGrpcMonix.TransportLayer,
      peer: PeerNode,
      networkId: String,
      blob: Blob,
      packetChunkSize: Int
  ): F[CommErr[Unit]] = {
    val chunkIt = Chunker.chunkIt[F](networkId, blob, packetChunkSize)
    transport
      .stream(Observable.fromIterator(chunkIt.toTask))
      .fromTask
      .attempt
      .map(processResponse(peer, _))
  }
}
