package coop.rchain.comm.transport

import scala.concurrent.duration.FiniteDuration

import cats._, cats.data._, cats.implicits._
import coop.rchain.comm.{CommError, PeerNode}, CommError.CommErr
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.shared._
import coop.rchain.comm.protocol.routing._

case class Blob(sender: PeerNode, packet: Packet)

trait TransportLayer[F[_]] {
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]]
  def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]]
  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]]
  def receive(
      dispatch: Protocol => F[CommunicationResponse],
      handleStreamed: Blob => F[Unit]
  ): F[Unit]
  def stream(peers: Seq[PeerNode], blob: Blob): F[Unit]
  def disconnect(peer: PeerNode): F[Unit]
  def shutdown(msg: Protocol): F[Unit]
}

object TransportLayer extends TransportLayerInstances {

  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L
}

sealed abstract class TransportLayerInstances {

  import CommunicationResponse._

  private implicit val logSource: LogSource = LogSource(this.getClass)

  implicit def eitherTTransportLayer[F[_]: Monad: Log](
      implicit evF: TransportLayer[F]
  ): TransportLayer[EitherT[F, CommError, ?]] =
    new TransportLayer[EitherT[F, CommError, ?]] {

      def roundTrip(
          peer: PeerNode,
          msg: Protocol,
          timeout: FiniteDuration
      ): EitherT[F, CommError, CommErr[Protocol]] =
        EitherT.liftF(evF.roundTrip(peer, msg, timeout))

      def send(peer: PeerNode, msg: Protocol): EitherT[F, CommError, CommErr[Unit]] =
        EitherT.liftF(evF.send(peer, msg))

      def broadcast(
          peers: Seq[PeerNode],
          msg: Protocol
      ): EitherT[F, CommError, Seq[CommErr[Unit]]] =
        EitherT.liftF(evF.broadcast(peers, msg))

      def stream(peers: Seq[PeerNode], blob: Blob): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.stream(peers, blob))

      def receive(
          dispatch: Protocol => EitherT[F, CommError, CommunicationResponse],
          handleStreamed: Blob => EitherT[F, CommError, Unit]
      ): EitherT[F, CommError, Unit] = {
        val dis: Protocol => F[CommunicationResponse] = msg =>
          dispatch(msg).value.flatMap {
            case Left(err) =>
              Log[F].error(s"Error while handling message. Error: ${err.message}") *> notHandled(
                err
              ).pure[F]
            case Right(m) => m.pure[F]
          }
        val hb: Blob => F[Unit] = (b) =>
          handleStreamed(b).value.flatMap {
            case Left(err) =>
              Log[F].error(s"Error while handling streamed Packet message. Error: ${err.message}")
            case Right(_) => ().pure[F]
          }
        EitherT.liftF(evF.receive(dis, hb))
      }

      def disconnect(peer: PeerNode): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.disconnect(peer))

      def shutdown(msg: Protocol): EitherT[F, CommError, Unit] = EitherT.liftF(evF.shutdown(msg))
    }
}
