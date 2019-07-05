package coop.rchain.comm.transport

import scala.util.{Either, Left, Right}

import cats.data.ReaderT
import cats.syntax.either._
import cats.syntax.option._

import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.{RoutingGrpcMonix, _}
import coop.rchain.comm.CommError._
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.Metrics

import io.grpc.{Status, StatusRuntimeException}
import monix.eval.Task
import monix.reactive.Observable

object GrpcTransport {

  type Request[A] = ReaderT[Task, RoutingGrpcMonix.TransportLayer, CommErr[A]]
  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  private def transport(
      peer: PeerNode
  )(
      request: RoutingGrpcMonix.TransportLayer => Task[TLResponse]
  ): Request[Unit] =
    ReaderT(stub => request(stub).attempt.map(processResponse(peer, _)))

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
            case p if p.isNoResponse => Right(())
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

  def send(peer: PeerNode, msg: Protocol)(implicit metrics: Metrics[Task]): Request[Unit] =
    for {
      _      <- ReaderT.liftF(metrics.incrementCounter("send"))
      result <- transport(peer)(_.send(TLRequest(msg.some)).timer("send-time"))
    } yield result

  def stream(networkId: String, peer: PeerNode, blob: Blob, packetChunkSize: Int): Request[Unit] =
    ReaderT(
      _.stream(Observable.fromIterator(Chunker.chunkIt(networkId, blob, packetChunkSize))).attempt
        .map(r => processError(peer, r.map(kp(()))))
    )
}
