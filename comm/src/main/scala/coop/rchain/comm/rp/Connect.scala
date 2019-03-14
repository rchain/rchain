package coop.rchain.comm.rp

import scala.concurrent.duration._

import cats._
import cats.implicits._
import cats.mtl._
import cats.effect._

import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm._
import coop.rchain.comm.CommError._
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits._
import coop.rchain.shared._

object Connect {

  type Connection            = PeerNode
  type Connections           = List[Connection]
  type ConnectionsCell[F[_]] = Cell[F, Connections]

  private implicit val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.connect")

  object ConnectionsCell {
    def apply[F[_]](implicit ev: ConnectionsCell[F]): ConnectionsCell[F] = ev
  }

  object Connections {
    def empty: Connections = List.empty[Connection]
    implicit class ConnectionsOps(connections: Connections) {

      def addConnAndReport[F[_]: Monad: Log: Metrics](connection: Connection): F[Connections] =
        addConn[F](connection) >>= (_.reportConn[F])

      def addConn[F[_]: Monad](connection: Connection): F[Connections] =
        addConn[F](List(connection))

      def addConn[F[_]: Monad](toBeAdded: List[Connection]): F[Connections] = {
        val ids = toBeAdded.map(_.id)
        val newConnections = connections.partition(peer => ids.contains(peer.id)) match {
          case (_, rest) => rest ++ toBeAdded
        }
        newConnections.pure[F]
      }

      def removeConnAndReport[F[_]: Monad: Log: Metrics](connection: Connection): F[Connections] =
        removeConn[F](connection) >>= (_.reportConn[F])

      def removeConn[F[_]: Monad](connection: Connection): F[Connections] =
        removeConn[F](List(connection))

      def removeConn[F[_]: Monad](toBeRemoved: List[Connection]): F[Connections] = {
        val ids = toBeRemoved.map(_.id)
        val newConnections = connections.partition(peer => ids.contains(peer.id)) match {
          case (_, rest) => rest
        }
        newConnections.pure[F]
      }

      def reportConn[F[_]: Monad: Log: Metrics]: F[Connections] = {
        val size = connections.size.toLong
        Log[F].info(s"Peers: $size") >>
          Metrics[F].setGauge("peers", size).as(connections)
      }
    }
  }

  import Connections._

  type RPConfState[F[_]] = MonadState[F, RPConf]
  type RPConfAsk[F[_]]   = ApplicativeAsk[F, RPConf]

  object RPConfAsk {
    def apply[F[_]](implicit ev: ApplicativeAsk[F, RPConf]): ApplicativeAsk[F, RPConf] = ev
  }

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def clearConnections[F[_]: Sync: Monad: Time: ConnectionsCell: RPConfAsk: TransportLayer: Log: Metrics]
    : F[Int] = {

    def sendHeartbeat(peer: PeerNode): F[(PeerNode, CommErr[Protocol])] =
      for {
        local   <- RPConfAsk[F].reader(_.local)
        timeout <- RPConfAsk[F].reader(_.defaultTimeout)
        hb      = heartbeat(local)
        res     <- TransportLayer[F].roundTrip(peer, hb, timeout)
      } yield (peer, res)

    def clear(connections: Connections): F[Int] =
      for {
        numOfConnectionsPinged <- RPConfAsk[F].reader(_.clearConnections.numOfConnectionsPinged)
        toPing                 = connections.take(numOfConnectionsPinged)
        results                <- toPing.traverse(sendHeartbeat)
        successfulPeers        = results.collect { case (peer, Right(_)) => peer }
        failedPeers            = results.collect { case (peer, Left(_)) => peer }
        _                      <- failedPeers.traverse(p => Log[F].info(s"Removing peer $p from connections"))
        _ <- ConnectionsCell[F].flatModify { connections =>
              connections.removeConn[F](toPing) >>= (_.addConn[F](successfulPeers))
            }
      } yield failedPeers.size

    for {
      connections <- ConnectionsCell[F].read
      max         <- RPConfAsk[F].reader(_.clearConnections.maxNumOfConnections)
      cleared     <- if (connections.size > ((max * 2) / 3)) clear(connections) else 0.pure[F]
      _           <- if (cleared > 0) ConnectionsCell[F].read >>= (_.reportConn[F]) else connections.pure[F]
    } yield cleared
  }

  def resetConnections[F[_]: Monad: ConnectionsCell: RPConfAsk: TransportLayer: Log: Metrics]
    : F[Unit] =
    ConnectionsCell[F].flatModify { connections =>
      for {
        local  <- RPConfAsk[F].reader(_.local)
        _      <- TransportLayer[F].broadcast(connections, disconnect(local))
        _      <- connections.traverse(TransportLayer[F].disconnect)
        result <- connections.removeConn[F](connections)
      } yield result
    }

  def findAndConnect[F[_]: Sync: Monad: Log: Time: Metrics: NodeDiscovery: ErrorHandler: ConnectionsCell: RPConfAsk](
      conn: (PeerNode, FiniteDuration) => F[Unit]
  ): F[List[PeerNode]] =
    for {
      connections      <- ConnectionsCell[F].read
      tout             <- RPConfAsk[F].reader(_.defaultTimeout)
      peers            <- NodeDiscovery[F].peers.map(p => (p.toSet -- connections).toList)
      responses        <- peers.traverse(p => ErrorHandler[F].attempt(conn(p, tout)))
      peersAndResonses = peers.zip(responses)
      _ <- peersAndResonses.traverse {
            case (peer, Left(error)) =>
              Log[F].debug(s"Failed to connect to ${peer.toAddress}. Reason: ${error.message}")
            case (peer, Right(_)) =>
              Log[F].info(s"Connected to ${peer.toAddress}.")
          }
    } yield peersAndResonses.filter(_._2.isRight).map(_._1)

  def connect[F[_]: Sync: Monad: Log: Time: Metrics: TransportLayer: ErrorHandler: ConnectionsCell: RPConfAsk](
      peer: PeerNode,
      timeout: FiniteDuration
  ): F[Unit] =
    (
      for {
        address  <- Sync[F].delay(peer.toAddress)
        _        <- Log[F].debug(s"Connecting to $address")
        _        <- Metrics[F].incrementCounter("connect")
        _        <- Log[F].debug(s"Initialize protocol handshake to $address")
        local    <- RPConfAsk[F].reader(_.local)
        ph       = protocolHandshake(local)
        response <- TransportLayer[F].send(peer, ph) >>= ErrorHandler[F].fromEither
      } yield ()
    ).timer("connect-time")

}
