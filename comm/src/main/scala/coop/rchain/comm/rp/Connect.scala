package coop.rchain.comm.rp

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.mtl._
import cats.syntax.all._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.implicits._
import coop.rchain.shared._

import scala.util.Random

object Connect {

  type Connection            = PeerNode
  type Connections           = List[Connection]
  type ConnectionsCell[F[_]] = Ref[F, Connections]

  implicit private val metricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "rp.connect")

  object ConnectionsCell {
    def apply[F[_]](implicit ev: ConnectionsCell[F]): ConnectionsCell[F] = ev

    def random[F[_]: Monad: ConnectionsCell](
        max: Int
    ): F[Connections] =
      for {
        peers <- ConnectionsCell[F].get
      } yield Random.shuffle(peers).take(max)
  }

  object Connections {
    def empty: Connections = List.empty[Connection]

    def reportConn[F[_]: Monad: Log: Metrics](connections: Connections): F[Unit] = {
      val size = connections.size
      for {
        _ <- Log[F].info(s"Peers: $size")
        _ <- Metrics[F].setGauge("peers", size.toLong)
      } yield ()
    }

    // TODO: rewrite extensions to cats syntax pattern (left after migration from old Cell trait)

    implicit class ConnectionsOps(connections: Connections) {
      def addConn(toBeAdded: List[Connection]): Connections = {
        val ids = toBeAdded.map(_.id)
        val newConnections = connections.partition(peer => ids.contains(peer.id)) match {
          case (_, rest) => rest ++ toBeAdded
        }
        newConnections
      }
      def addConn(conn: Connection): Connections = addConn(List(conn))

      def removeConn(toBeRemoved: List[Connection]): Connections = {
        val ids = toBeRemoved.map(_.id)
        val newConnections = connections.partition(peer => ids.contains(peer.id)) match {
          case (_, rest) => rest
        }
        newConnections
      }
      def removeConn(conn: Connection): Connections = removeConn(List(conn))

      def refreshConn(connection: Connection): Connections = {
        val newConnections = connections.partition(_.id == connection.id) match {
          case (peer, rest) if peer.isEmpty => rest
          case (peer, rest)                 => rest ++ peer
        }
        newConnections
      }
    }

    implicit class ConnectionsRefOps[F[_]: Monad: Log: Metrics](connRef: Ref[F, Connections]) {
      def addConnAndReport(conn: Connection): F[Connections] =
        for {
          connections <- connRef.updateAndGet(_.addConn(List(conn)))
          _           <- reportConn(connections)
        } yield connections

      def removeConnAndReport(conn: Connection): F[Connections] =
        for {
          connections <- connRef.updateAndGet(_.removeConn(List(conn)))
          _           <- reportConn(connections)
        } yield connections
    }
  }

  import Connections._

  type RPConfState[F[_]] = Ref[F, RPConf]

  object RPConfState {
    def apply[F[_]](implicit instance: Ref[F, RPConf]): instance.type = instance
  }

  type RPConfAsk[F[_]] = ApplicativeAsk[F, RPConf]

  object RPConfAsk {
    def apply[F[_]](implicit instance: ApplicativeAsk[F, RPConf]): instance.type = instance
  }

  implicit private val logSource: LogSource = LogSource(this.getClass)

  def clearConnections[F[_]: Sync: ConnectionsCell: RPConfAsk: TransportLayer: Log: Metrics]
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
        _ <- ConnectionsCell[F].update { connections =>
              connections.removeConn(toPing).addConn(successfulPeers)
            }
      } yield failedPeers.size

    for {
      connections <- ConnectionsCell[F].get
      cleared     <- clear(connections)
      _           <- (ConnectionsCell[F].get >>= reportConn[F]).whenA(cleared > 0)
    } yield cleared
  }

  def resetConnections[F[_]: Monad: ConnectionsCell]: F[Unit] =
    ConnectionsCell[F].update(c => c.removeConn(c))

  def findAndConnect[F[_]: Monad: Log: NodeDiscovery: ConnectionsCell](
      conn: PeerNode => F[CommErr[Unit]]
  ): F[List[PeerNode]] =
    for {
      connections <- ConnectionsCell[F].get.map(_.toSet)
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
