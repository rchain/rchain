package coop.rchain.p2p.effects

import java.net.SocketAddress
import scala.concurrent.duration.{Duration, MILLISECONDS}

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.{PeerNode, ProtocolMessage, ProtocolNode}

trait TransportLayer[F[_]] {
  // TODO rename
  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration): F[CommErr[ProtocolMessage]]
  // TODO return PeerNode
  def local: F[ProtocolNode]
  // TODO remove ProtocolMessage, use raw messages from protocol
  def commSend(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]]
  def broadcast(msg: ProtocolMessage): F[Seq[CommErr[Unit]]]
  def receive(dispatch: ProtocolMessage => F[Unit]): F[Unit]
}

object TransportLayer extends TransportLayerInstances {
  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L
}

sealed abstract class TransportLayerInstances {

  implicit def eitherTTransportLayer[E, F[_]: Monad: Log](
      implicit evF: TransportLayer[F]): TransportLayer[EitherT[F, E, ?]] =
    new TransportLayer[EitherT[F, E, ?]] {
      def roundTrip(msg: ProtocolMessage,
                    remote: ProtocolNode,
                    timeout: Duration): EitherT[F, E, CommErr[ProtocolMessage]] =
        EitherT.liftF(evF.roundTrip(msg, remote, timeout))

      def local: EitherT[F, E, ProtocolNode] =
        EitherT.liftF(evF.local)
      def commSend(msg: ProtocolMessage, p: PeerNode): EitherT[F, E, CommErr[Unit]] =
        EitherT.liftF(evF.commSend(msg, p))

      def broadcast(msg: ProtocolMessage): EitherT[F, E, Seq[CommErr[Unit]]] =
        EitherT.liftF(evF.broadcast(msg))
      def receive(dispatch: ProtocolMessage => EitherT[F, E, Unit]): EitherT[F, E, Unit] = {
        val dis: ProtocolMessage => F[Unit] = msg =>
          dispatch(msg).value.map(_ match {
            case Left(err)   => Log[F].error(s"Error while handling message. Error: $err")
            case Right(unit) => unit
          })
        EitherT.liftF(evF.receive(dis))
      }

    }
}
