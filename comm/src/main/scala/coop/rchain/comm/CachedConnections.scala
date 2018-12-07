package coop.rchain.comm

import cats.MonadError
import cats.effect.Concurrent
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import coop.rchain.catscontrib.ski.kp
import coop.rchain.shared.Cell

import io.grpc.ManagedChannel
import scala.language.higherKinds

import monix.execution.Cancelable

class CachedConnections[F[_], T](val cell: Transport.TransportCell[F])(
    clientChannel: PeerNode => F[ManagedChannel]
)(implicit E: MonadError[F, Throwable]) {

  def connection(peer: PeerNode, enforce: Boolean): F[ManagedChannel] =
    cell.modify { s =>
      if (s.shutdown && !enforce)
        E.raiseError(new RuntimeException("The transport layer has been shut down")).as(s)
      else
        for {
          c <- s.connections.get(peer).fold(clientChannel(peer))(_.pure[F])
        } yield s.copy(connections = s.connections + (peer -> c))
    } >>= kp(cell.read.map(_.connections.apply(peer)))
}

object CachedConnections {
  type ConnectionsCache[F[_], T] = (PeerNode => F[ManagedChannel]) => CachedConnections[F, T]

  def apply[F[_]: Concurrent, T]: F[ConnectionsCache[F, T]] =
    for {
      connections <- Cell.mvarCell[F, TransportState](TransportState.empty)
    } yield new CachedConnections[F, T](connections)(_)
}

object Transport {
  type Connection          = ManagedChannel
  type Connections         = Map[PeerNode, Connection]
  type TransportCell[F[_]] = Cell[F, TransportState]
}

case class TransportState(
    connections: Transport.Connections = Map.empty,
    server: Option[Cancelable] = None,
    clientQueue: Option[Cancelable] = None,
    shutdown: Boolean = false
)

object TransportState {
  def empty: TransportState = TransportState()
}
