package coop.rchain.comm.transport

import scala.concurrent.duration.FiniteDuration

import cats._, cats.data._, cats.implicits._
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.{PeerNode, ProtocolMessage}
import coop.rchain.shared._

trait TransportLayer[F[_]] {
  // TODO return PeerNode, do we still neeed it?
  def local: F[PeerNode]
  def roundTrip(peer: PeerNode,
                msg: ProtocolMessage,
                timeout: FiniteDuration): F[CommErr[ProtocolMessage]]
  // TODO remove ProtocolMessage, use raw messages from protocol
  def send(peer: PeerNode, msg: ProtocolMessage): F[CommErr[Unit]]
  def broadcast(peers: Seq[PeerNode], msg: ProtocolMessage): F[Seq[CommErr[Unit]]]
  def receive(dispatch: ProtocolMessage => F[CommunicationResponse]): F[Unit]
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
                    msg: ProtocolMessage,
                    timeout: FiniteDuration): EitherT[F, E, CommErr[ProtocolMessage]] =
        EitherT.liftF(evF.roundTrip(peer, msg, timeout))

      def send(peer: PeerNode, msg: ProtocolMessage): EitherT[F, E, CommErr[Unit]] =
        EitherT.liftF(evF.send(peer, msg))

      def broadcast(peers: Seq[PeerNode], msg: ProtocolMessage): EitherT[F, E, Seq[CommErr[Unit]]] =
        EitherT.liftF(evF.broadcast(peers, msg))

      def receive(dispatch: ProtocolMessage => EitherT[F, E, CommunicationResponse])
        : EitherT[F, E, Unit] = {
        val dis: ProtocolMessage => F[CommunicationResponse] = msg =>
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
