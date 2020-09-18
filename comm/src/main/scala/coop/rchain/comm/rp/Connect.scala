package coop.rchain.comm.rp

import scala.concurrent.duration._
import scala.util.Random

import cats._
import cats.effect._
import cats.implicits._
import cats.mtl._

import coop.rchain.comm._
import coop.rchain.comm.CommError._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits._
import coop.rchain.shared._

object Connect {

  type Connection            = PeerNode
  type Connections           = List[Connection]
  type ConnectionsCell[F[_]] = Cell[F, Connections]

  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.connect")

  object ConnectionsCell {
    def apply[F[_]](implicit ev: ConnectionsCell[F]): ConnectionsCell[F] = ev

    def random[F[_]: Monad: ConnectionsCell](
        max: Int
    ): F[Connections] =
      for {
        peers <- ConnectionsCell[F].read
      } yield Random.shuffle(peers).take(max)
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

      def refreshConn[F[_]: Monad](connection: Connection): F[Connections] = {
        val newConnections = connections.partition(_.id == connection.id) match {
          case (peer, rest) if peer.isEmpty => rest
          case (peer, rest)                 => rest ++ peer
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

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def clearConnections[F[_]: Sync: Monad: Time: ConnectionsCell: RPConfAsk: TransportLayer: Log: Metrics]
      : F[Int] = {

    def sendHeartbeat(peer: PeerNode): F[(PeerNode, CommErr[Unit])] =
      for {
        conf <- RPConfAsk[F].ask
        hb   = heartbeat(conf.local, conf.networkId)
        res  <- TransportLayer[F].send(peer, hb)
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
      cleared     <- clear(connections)
      _           <- if (cleared > 0) ConnectionsCell[F].read >>= (_.reportConn[F]) else connections.pure[F]
    } yield cleared
  }

  def resetConnections[F[_]: Monad: ConnectionsCell]: F[Unit] =
    ConnectionsCell[F].flatModify(c => c.removeConn[F](c))

  def findAndConnect[F[_]: Monad: Log: NodeDiscovery: ConnectionsCell](
      conn: PeerNode => F[CommErr[Unit]]
  ): F[List[PeerNode]] =
    for {
      connections <- ConnectionsCell[F].read.map(_.toSet)
      peers       <- NodeDiscovery[F].peers.map(_.filterNot(connections.contains).toList)
      responses   <- peers.traverse(conn)
      _ <- responses.collect {
            case Left(WrongNetwork(peer, msg)) =>
              Log[F].warn(s"Can't connect to peer $peer. $msg")
          }.sequence
      peersAndResponses = peers.zip(responses)
    } yield peersAndResponses.filter(_._2.isRight).map(_._1)

  def connect[F[_]: Monad: Log: Metrics: TransportLayer: RPConfAsk](
      peer: PeerNode
  ): F[CommErr[Unit]] =
    (
      for {
        address <- peer.toAddress.pure[F]
        //_       <- Log[F].debug(s"Connecting to $address")
        _ <- Metrics[F].incrementCounter("connect")
        //_       <- Log[F].debug(s"Initialize protocol handshake to $address")
        conf <- RPConfAsk[F].ask
        ph   = protocolHandshake(conf.local, conf.networkId)
        res  <- TransportLayer[F].send(peer, ph)
      } yield res
    ).timer("connect-time")

}
