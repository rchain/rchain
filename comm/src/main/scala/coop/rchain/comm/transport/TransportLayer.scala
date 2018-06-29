package coop.rchain.comm.transport

import scala.concurrent.duration.FiniteDuration

import cats._, cats.data._, cats.implicits._
import coop.rchain.comm.CommError
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.{PeerNode, ProtocolHelper}
import coop.rchain.shared._
import coop.rchain.comm.protocol.routing._

trait TransportLayer[F[_]] {
  // TODO return PeerNode, do we still neeed it?
  def local: F[PeerNode]
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]]
  // TODO remove ProtocolMessage, use raw messages from protocol
  def send(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[Unit]
  def broadcast(peers: Seq[PeerNode], msg: Protocol, timeout: FiniteDuration): F[Unit]
  def safeBroadcast(peers: Seq[PeerNode],
                    msg: Protocol,
                    timeout: FiniteDuration): F[Map[PeerNode, Option[CommError]]]
  def receive(dispatch: Protocol => F[CommunicationResponse]): F[Unit]
  def disconnect(peer: PeerNode): F[Unit]

}

object TransportLayer extends TransportLayerInstances {

  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L
}

sealed abstract class TransportLayerInstances {

  import CommunicationResponse._

  private implicit val logSource: LogSource = LogSource(this.getClass)

  implicit def eitherTTransportLayer[E, F[_]: Monad: Log](
      implicit evF: TransportLayer[F]): TransportLayer[EitherT[F, E, ?]] =
    new TransportLayer[EitherT[F, E, ?]] {

      def local: EitherT[F, E, PeerNode] =
        EitherT.liftF(evF.local)

      def roundTrip(peer: PeerNode,
                    msg: Protocol,
                    timeout: FiniteDuration): EitherT[F, E, CommErr[Protocol]] =
        EitherT.liftF(evF.roundTrip(peer, msg, timeout))

      def send(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): EitherT[F, E, Unit] =
        EitherT.liftF(evF.send(peer, msg, timeout))

      def broadcast(peers: Seq[PeerNode],
                    msg: Protocol,
                    timeout: FiniteDuration): EitherT[F, E, Unit] =
        EitherT.liftF(evF.broadcast(peers, msg, timeout))

      def safeBroadcast(peers: Seq[PeerNode],
                        msg: Protocol,
                        timeout: FiniteDuration): EitherT[F, E, Map[PeerNode, Option[CommError]]] =
        EitherT.liftF(evF.safeBroadcast(peers, msg, timeout))

      def receive(
          dispatch: Protocol => EitherT[F, E, CommunicationResponse]): EitherT[F, E, Unit] = {
        val dis: Protocol => F[CommunicationResponse] = msg =>
          dispatch(msg).value.flatMap {
            case Left(err) =>
              Log[F].error(s"Error while handling message. Error: $err") *> notHandled.pure[F]
            case Right(m) => m.pure[F]
        }
        EitherT.liftF(evF.receive(dis))
      }

      def disconnect(peer: PeerNode): EitherT[F, E, Unit] = EitherT.liftF(evF.disconnect(peer))
    }
}
