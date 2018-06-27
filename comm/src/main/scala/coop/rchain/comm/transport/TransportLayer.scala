package coop.rchain.comm.transport

import coop.rchain.p2p.effects._
import java.net.SocketAddress
import scala.concurrent.duration.{Duration, MILLISECONDS}

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.CommErr
import coop.rchain.comm.{PeerNode, ProtocolMessage}
import coop.rchain.shared._
import coop.rchain.comm.protocol.routing._

trait TransportLayer[F[_]] {
  def roundTrip(msg: Protocol, remote: PeerNode, timeout: Duration): F[CommErr[Protocol]]
  // TODO local should be avaialble via ApplicativeAsk, not be part of TransportLayer
  def local: F[PeerNode]
  def send(msg: Protocol, peer: PeerNode): F[CommErr[Unit]]
  def broadcast(msg: Protocol, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]]
  def receive(dispatch: Protocol => F[CommunicationResponse]): F[Unit]
}

object TransportLayer extends TransportLayerInstances {

  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L
}

sealed abstract class TransportLayerInstances {

  import CommunicationResponse._

  implicit def eitherTTransportLayer[E, F[_]: Monad: Log](
      implicit evF: TransportLayer[F]): TransportLayer[EitherT[F, E, ?]] =
    new TransportLayer[EitherT[F, E, ?]] {
      def roundTrip(msg: Protocol,
                    remote: PeerNode,
                    timeout: Duration): EitherT[F, E, CommErr[Protocol]] =
        EitherT.liftF(evF.roundTrip(msg, remote, timeout))

      def local: EitherT[F, E, PeerNode] =
        EitherT.liftF(evF.local)
      def send(msg: Protocol, p: PeerNode): EitherT[F, E, CommErr[Unit]] =
        EitherT.liftF(evF.send(msg, p))

      def broadcast(msg: Protocol, peers: Seq[PeerNode]): EitherT[F, E, Seq[CommErr[Unit]]] =
        EitherT.liftF(evF.broadcast(msg, peers))
      def receive(
          dispatch: Protocol => EitherT[F, E, CommunicationResponse]): EitherT[F, E, Unit] = {
        val dis: Protocol => F[CommunicationResponse] = msg =>
          dispatch(msg).value.flatMap(_ match {
            case Left(err) =>
              Log[F].error(s"Error while handling message. Error: $err") *> notHandled.pure[F]
            case Right(msg) => msg.pure[F]
          })
        EitherT.liftF(evF.receive(dis))
      }

    }
}
