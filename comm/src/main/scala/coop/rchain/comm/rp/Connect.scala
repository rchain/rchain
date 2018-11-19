package coop.rchain.comm.rp

import cats._
import cats.implicits._
import cats.mtl._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport._
import coop.rchain.metrics.Metrics
import coop.rchain.shared._

import scala.concurrent.duration._

object Connect {

  type Connection            = PeerNode
  type Connections           = List[Connection]
  type ConnectionsCell[F[_]] = Cell[F, Connections]

  object ConnectionsCell {
    def apply[F[_]](implicit ev: ConnectionsCell[F]): ConnectionsCell[F] = ev
  }

  object Connections {
    def empty: Connections = List.empty[Connection]
    implicit class ConnectionsOps(connections: Connections) {

      def addConn[F[_]: Monad: Log: Metrics](connection: Connection): F[Connections] =
        addConn[F](List(connection))

      def addConn[F[_]: Monad: Log: Metrics](toBeAdded: List[Connection]): F[Connections] = {
        val ids = toBeAdded.map(_.id)
        val newConnections = connections.partition(peer => ids.contains(peer.id)) match {
          case (_, rest) => rest ++ toBeAdded
        }
        val size = newConnections.size.toLong
        Log[F].info(s"Peers: $size.") *>
          Metrics[F].setGauge("peers", size).as(newConnections)
      }

      def removeConn[F[_]: Monad: Log: Metrics](connection: Connection): F[Connections] =
        removeConn[F](List(connection))

      def removeConn[F[_]: Monad: Log: Metrics](toBeRemoved: List[Connection]): F[Connections] = {
        val ids = toBeRemoved.map(_.id)
        val newConnections = connections.partition(peer => ids.contains(peer.id)) match {
          case (_, rest) => rest
        }
        val size = newConnections.size.toLong
        Log[F].info(s"Peers: $size.") *>
          Metrics[F].setGauge("peers", size).as(newConnections)
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

  def clearConnections[F[_]: Capture: Monad: Time: ConnectionsCell: RPConfAsk: TransportLayer: Log: Metrics]
    : F[Int] = {

    def sendHeartbeat(peer: PeerNode, local: PeerNode): F[(PeerNode, CommErr[Protocol])] =
      for {
        timeout <- RPConfAsk[F].reader(_.defaultTimeout)
        hb      = heartbeat(local)
        res     <- TransportLayer[F].roundTrip(peer, hb, timeout)
      } yield (peer, res)

    def clear(connections: Connections, local: PeerNode): F[Int] =
      for {
        toPingCount     <- RPConfAsk[F].reader(_.clearConnections.numOfConnectionsPinged)
        toPing          = connections.take(toPingCount)
        results         <- toPing.traverse(p => sendHeartbeat(p, local))
        successfulPeers = results.collect { case (peer, Right(_)) => peer }
        failedPeers     = results.collect { case (peer, Left(_)) => peer }
        _               <- ConnectionsCell[F].modify(_.removeConn[F](toPing) >>= (_.addConn[F](successfulPeers)))
      } yield failedPeers.size

    def reset(connections: Connections, prevLocal: PeerNode, newLocal: PeerNode): F[Int] =
      for {
        _ <- TransportLayer[F].broadcast(connections, disconnect(prevLocal))
        _ <- connections.traverse(TransportLayer[F].disconnect)
        _ <- ConnectionsCell[F].modify(_.removeConn[F](connections))
      } yield connections.size

    def clearOrReset(
        connections: Connections,
        maxNumOfConnections: Int,
        local: PeerNode,
        dynamicLocal: PeerNode
    ): F[Int] =
      if (local.endpoint != dynamicLocal.endpoint)
        reset(connections, local, dynamicLocal)
      else if (connections.size > ((maxNumOfConnections * 2) / 3))
        clear(connections, local)
      else 0.pure[F]

    for {
      connections <- ConnectionsCell[F].read
      local       <- RPConfAsk[F].reader(_.local)
      dynLocal    <- RPConfAsk[F].reader(_.dynamicLocal)
      max         <- RPConfAsk[F].reader(_.clearConnections.maxNumOfConnections)
      cleared     <- clearOrReset(connections, max, local, dynLocal)
    } yield cleared
  }

  def findAndConnect[F[_]: Capture: Monad: Log: Time: Metrics: NodeDiscovery: ErrorHandler: ConnectionsCell: RPConfAsk](
      conn: (PeerNode, FiniteDuration) => F[Unit]
  ): F[List[PeerNode]] =
    for {
      connections      <- ConnectionsCell[F].read
      tout             <- RPConfAsk[F].reader(_.defaultTimeout)
      peers            <- NodeDiscovery[F].peers.map(p => (p.toSet -- connections).toList)
      responses        <- peers.traverse(conn(_, tout).attempt)
      peersAndResonses = peers.zip(responses)
      _ <- peersAndResonses.traverse {
            case (peer, Left(error)) =>
              Log[F].debug(s"Failed to connect to ${peer.toAddress}. Reason: ${error.message}")
            case (peer, Right(_)) =>
              Log[F].info(s"Connected to ${peer.toAddress}.")
          }
    } yield peersAndResonses.filter(_._2.isRight).map(_._1)

  def connect[F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: ConnectionsCell: RPConfAsk](
      peer: PeerNode,
      timeout: FiniteDuration
  ): F[Unit] =
    for {
      tss      <- Time[F].currentMillis
      peerAddr = peer.toAddress
      _        <- Log[F].debug(s"Connecting to $peerAddr")
      _        <- Metrics[F].incrementCounter("connects")
      _        <- Log[F].debug(s"Initialize protocol handshake to $peerAddr")
      local    <- RPConfAsk[F].reader(_.local)
      ph       = protocolHandshake(local)
      phsresp  <- TransportLayer[F].roundTrip(peer, ph, timeout * 2) >>= ErrorHandler[F].fromEither
      _ <- Log[F].debug(
            s"Received protocol handshake response from ${ProtocolHelper.sender(phsresp)}."
          )
      _   <- ConnectionsCell[F].modify(_.addConn[F](peer))
      tsf <- Time[F].currentMillis
      _   <- Metrics[F].record("connect-time-ms", tsf - tss)
    } yield ()

}
