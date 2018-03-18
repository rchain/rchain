package coop.rchain.catscontrib

import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.comm._, CommError._
import cats._, cats.data._, cats.implicits._
import Catscontrib._

trait Communication[F[_]] {
  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration = Duration(500, MILLISECONDS)): F[CommErr[ProtocolMessage]]
  def local: F[ProtocolNode]
  def commSend(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]]
  def addNode(node: PeerNode): F[Unit]
  def broadcast(msg: ProtocolMessage): F[Seq[CommErr[Unit]]]
  def findMorePeers(limit: Int): F[Seq[PeerNode]]
  def countPeers: F[Int]
  // TODO this has to be refactored, dispatch should become part of communication, receiver part of business logic
  def receiver: F[Unit]
}

object Communication extends CommunicationInstances {
  def apply[F[_]](implicit L: Communication[F]): Communication[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: Communication[F]): Communication[T[F, ?]] =
    new Communication[T[F, ?]] {
      def roundTrip(
          msg: ProtocolMessage,
          remote: ProtocolNode,
          timeout: Duration = Duration(500, MILLISECONDS)): T[F, CommErr[ProtocolMessage]] =
        C.roundTrip(msg, remote, timeout).liftM[T]
      def local: T[F, ProtocolNode] = C.local.liftM[T]
      def commSend(msg: ProtocolMessage, p: PeerNode): T[F, CommErr[Unit]] =
        C.commSend(msg, p).liftM[T]
      def addNode(node: PeerNode): T[F, Unit]                       = C.addNode(node).liftM[T]
      def broadcast(msg: ProtocolMessage): T[F, Seq[CommErr[Unit]]] = C.broadcast(msg).liftM[T]
      def findMorePeers(limit: Int): T[F, Seq[PeerNode]]            = C.findMorePeers(limit).liftM[T]
      def countPeers: T[F, Int]                                     = C.countPeers.liftM[T]
      def receiver: T[F, Unit]                                      = C.receiver.liftM[T]
    }
}

sealed abstract class CommunicationInstances {
  implicit def eitherTCommunication[E, F[_]: Monad: Communication[?[_]]]
    : Communication[EitherT[F, E, ?]] =
    Communication.forTrans[F, EitherT[?[_], E, ?]]
}
