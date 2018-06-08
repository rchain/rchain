package coop.rchain.p2p.effects

import java.net.SocketAddress
import scala.concurrent.duration.{Duration, MILLISECONDS}

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.{PeerNode, ProtocolMessage}

trait TransportLayer[F[_]] {

  def roundTrip(msg: ProtocolMessage,
                remote: PeerNode,
                timeout: Duration): F[CommErr[ProtocolMessage]]
  // TODO return PeerNode, do we still neeed it?
  def local: F[PeerNode]
  // TODO remove ProtocolMessage, use raw messages from protocol
  def send(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]]
  def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]]
  def receive(dispatch: ProtocolMessage => F[CommunicationResponse]): F[Unit]
}

object TransportLayer extends TransportLayerInstances {

  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L
}

sealed abstract class TransportLayerInstances {

  import CommunicationResponse._

  implicit def eitherTTransportLayer[E, F[_]: Monad: Log](
      implicit evF: TransportLayer[F]): TransportLayer[EitherT[F, E, ?]] =
    new TransportLayer[EitherT[F, E, ?]] {
      def roundTrip(msg: ProtocolMessage,
                    remote: PeerNode,
                    timeout: Duration): EitherT[F, E, CommErr[ProtocolMessage]] =
        EitherT.liftF(evF.roundTrip(msg, remote, timeout))

      def local: EitherT[F, E, PeerNode] =
        EitherT.liftF(evF.local)
      def send(msg: ProtocolMessage, p: PeerNode): EitherT[F, E, CommErr[Unit]] =
        EitherT.liftF(evF.send(msg, p))

      def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): EitherT[F, E, Seq[CommErr[Unit]]] =
        EitherT.liftF(evF.broadcast(msg, peers))
      def receive(dispatch: ProtocolMessage => EitherT[F, E, CommunicationResponse])
        : EitherT[F, E, Unit] = {
        val dis: ProtocolMessage => F[CommunicationResponse] = msg =>
          dispatch(msg).value.flatMap(_ match {
            case Left(err) =>
              Log[F].error(s"Error while handling message. Error: $err") *> notHandled.pure[F]
            case Right(msg) => msg.pure[F]
          })
        EitherT.liftF(evF.receive(dis))
      }

    }
}
