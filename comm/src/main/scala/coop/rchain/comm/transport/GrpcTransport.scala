package coop.rchain.comm.transport

import scala.util.{Either, Left, Right}

import cats.data.ReaderT
import cats.syntax.either._
import cats.syntax.option._

import coop.rchain.comm._
import coop.rchain.comm.protocol.routing.{RoutingGrpcMonix, _}
import coop.rchain.comm.CommError._
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.catscontrib.ski._

import io.grpc.{Status, StatusRuntimeException}
import monix.eval.Task
import monix.reactive.Observable

object GrpcTransport {

  type Request[A] = ReaderT[Task, RoutingGrpcMonix.TransportLayer, CommErr[A]]
  private implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.transport")

  private def transport(
      peer: PeerNode
  )(
      request: RoutingGrpcMonix.TransportLayer => Task[TLResponse]
  ): Request[Option[Protocol]] =
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

  private def processResponse(
      peer: PeerNode,
      response: Either[Throwable, TLResponse]
  ): CommErr[Option[Protocol]] =
    processError(peer, response)
      .flatMap(
        tlr =>
          tlr.payload match {
            case p if p.isProtocol   => Right(Some(tlr.getProtocol))
            case p if p.isNoResponse => Right(None)
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
        case PeerTimeout()     => CommError.timeout
        case PeerUnavailable() => peerUnavailable(peer)
        case e                 => protocolException(e)
      }

  def roundTrip(peer: PeerNode, msg: Protocol)(
      implicit metrics: Metrics[Task]
  ): Request[Protocol] =
    for {
      _ <- ReaderT.liftF(metrics.incrementCounter("round-trip"))
      result <- transport(peer)(
                 _.ask(TLRequest(msg.some))
                   .timer("round-trip-time")
               ).map(_.flatMap {
                 case Some(p) => Right(p)
                 case _ =>
                   Left(internalCommunicationError("Was expecting message, nothing arrived"))
               })
    } yield result

  def send(peer: PeerNode, msg: Protocol)(implicit metrics: Metrics[Task]): Request[Unit] =
    for {
      _ <- ReaderT.liftF(metrics.incrementCounter("send"))
      result <- transport(peer)(
                 _.tell(TLRequest(msg.some))
                   .timer("send-time")
               ).map(_.flatMap {
                 case Some(p) =>
                   Left(internalCommunicationError(s"Was expecting no message. Response: $p"))
                 case _ => Right(())
               })
    } yield result

  def stream(peer: PeerNode, blob: Blob, messageSize: Int): Request[Unit] =
    ReaderT(
      _.stream(Observable.fromIterator(Chunker.chunkIt(blob, messageSize))).attempt
        .map(r => processError(peer, r.map(kp(()))))
    )

}
