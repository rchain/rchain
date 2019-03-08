package coop.rchain.comm.transport

import scala.concurrent.duration.FiniteDuration

import cats._, cats.data._, cats.implicits._
import coop.rchain.comm.{CommError, PeerNode}, CommError.CommErr
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.shared._
import coop.rchain.comm.protocol.routing._

final case class Blob(sender: PeerNode, packet: Packet)

trait TransportLayer[F[_]] {
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]]
  def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]]
  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]]
  def stream(peers: Seq[PeerNode], blob: Blob): F[Unit]
  def disconnect(peer: PeerNode): F[Unit]
}

object TransportLayer extends TransportLayerInstances {

  def apply[F[_]](implicit L: TransportLayer[F]): TransportLayer[F] = L
}

sealed abstract class TransportLayerInstances {
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

      def disconnect(peer: PeerNode): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.disconnect(peer))
    }
}
