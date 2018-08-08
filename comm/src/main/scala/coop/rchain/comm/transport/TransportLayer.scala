package coop.rchain.comm.transport

import scala.concurrent.duration.FiniteDuration

import cats._, cats.data._, cats.implicits._
import coop.rchain.comm.CommError, CommError.CommErr
import coop.rchain.comm.{PeerNode, ProtocolHelper}
import coop.rchain.shared._
import coop.rchain.comm.protocol.routing._

// TODO TransportLayer should be parametized by type of messages it is able to send
trait TransportLayer[F[_]] {
  def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]]
  def send(peer: PeerNode, msg: Protocol): F[Unit]
  def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Unit]
  def receive(dispatch: Protocol => F[CommunicationResponse]): F[Unit]
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
      implicit evF: TransportLayer[F]): TransportLayer[EitherT[F, CommError, ?]] =
    new TransportLayer[EitherT[F, CommError, ?]] {

      def roundTrip(peer: PeerNode,
                    msg: Protocol,
                    timeout: FiniteDuration): EitherT[F, CommError, CommErr[Protocol]] =
        EitherT.liftF(evF.roundTrip(peer, msg, timeout))

      def send(peer: PeerNode, msg: Protocol): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.send(peer, msg))

      def broadcast(peers: Seq[PeerNode], msg: Protocol): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.broadcast(peers, msg))

      def receive(dispatch: Protocol => EitherT[F, CommError, CommunicationResponse])
        : EitherT[F, CommError, Unit] = {
        val dis: Protocol => F[CommunicationResponse] = msg =>
          dispatch(msg).value.flatMap {
            case Left(err) =>
              Log[F].error(s"Error while handling message. Error: ${err.message}") *> notHandled(
                err).pure[F]
            case Right(m) => m.pure[F]
        }
        EitherT.liftF(evF.receive(dis))
      }

      def disconnect(peer: PeerNode): EitherT[F, CommError, Unit] =
        EitherT.liftF(evF.disconnect(peer))

      def shutdown(msg: Protocol): EitherT[F, CommError, Unit] = EitherT.liftF(evF.shutdown(msg))
    }
}
