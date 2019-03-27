package coop.rchain.comm.transport

import cats._
import cats.data._

import coop.rchain.comm.{CommError, PeerNode}
import CommError.CommErr
import coop.rchain.comm.protocol.routing._
import coop.rchain.shared._

final case class Blob(sender: PeerNode, packet: Packet)

trait TransportLayer[F[_]] {
  def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]]
  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]]
  def stream(peer: PeerNode, blob: Blob): F[Unit]
  def stream(peers: Seq[PeerNode], blob: Blob): F[Unit]
}

object TransportLayer extends TransportLayerInstances {
  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L
}

sealed abstract class TransportLayerInstances {
  implicit def eitherTTransportLayer[F[_]: Monad: Log](
      implicit evF: TransportLayer[F]
  ): TransportLayer[EitherT[F, CommError, ?]] =
    new TransportLayer[EitherT[F, CommError, ?]] {

      def send(peer: PeerNode, msg: Protocol): EitherT[F, CommError, CommErr[Unit]] =
        EitherT.liftF(evF.send(peer, msg))

      def broadcast(
          peers: Seq[PeerNode],
          msg: Protocol
      ): EitherT[F, CommError, Seq[CommErr[Unit]]] =
        EitherT.liftF(evF.broadcast(peers, msg))

      def stream(peer: PeerNode, blob: Blob): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.stream(peer, blob))

      def stream(peers: Seq[PeerNode], blob: Blob): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.stream(peers, blob))
    }
}
