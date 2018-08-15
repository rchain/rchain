package coop.rchain.comm.rp

import coop.rchain.p2p.effects._
import coop.rchain.comm.discovery._
import scala.concurrent.duration._
import com.google.protobuf.any.{Any => AnyProto}
import coop.rchain.comm.protocol.routing
import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing.{Protocol => RoutingProtocol}
import coop.rchain.comm.protocol.rchain._
import coop.rchain.metrics.Metrics

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import cats.mtl._
import coop.rchain.comm.transport._, CommunicationResponse._, CommMessages._
import coop.rchain.shared._
import coop.rchain.comm.CommError._

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
        connections
          .contains(connection)
          .fold(
            connections.pure[F],
            Log[F].info(s"Peers: ${connections.size + 1}.").as(connection :: connections) >>= (
                conns => Metrics[F].setGauge("peers", conns.size.toLong).as(conns))
          )
      def removeConn[F[_]: Monad: Log: Metrics](connection: Connection): F[Connections] =
        for {
          result <- connections.filter(_ != connection).pure[F]
          count  = result.size.toLong
          _      <- Log[F].info(s"Peers: $count.") >>= (_ => Metrics[F].setGauge("peers", count))
        } yield result

      def removeAndAddAtEnd[F[_]: Monad: Log: Metrics](toRemove: List[PeerNode],
                                                       toAddAtEnd: List[PeerNode]): F[Connections] =
        for {
          result <- (connections.filter(conn => !toRemove.contains(conn)) ++ toAddAtEnd).pure[F]
          count  = result.size.toLong
          _      <- Log[F].info(s"Peers: $count.") >>= (_ => Metrics[F].setGauge("peers", count))
        } yield result
    }
  }
  import Connections._

  type RPConfAsk[F[_]] = ApplicativeAsk[F, RPConf]
  object RPConfAsk {
    def apply[F[_]](implicit ev: ApplicativeAsk[F, RPConf]): ApplicativeAsk[F, RPConf] = ev
  }

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def clearConnections[
      F[_]: Capture: Monad: ConnectionsCell: RPConfAsk: TransportLayer: Log: Metrics]: F[Int] = {

    def sendHeartbeat(peer: PeerNode): F[(PeerNode, CommErr[RoutingProtocol])] =
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
        results                <- toPing.traverse(sendHeartbeat(_))
        successfulPeers        = results.collect { case (peer, Right(_)) => peer }
        failedPeers            = results.collect { case (peer, Left(_)) => peer }
        _                      <- ConnectionsCell[F].modify(_.removeAndAddAtEnd[F](toPing, successfulPeers))
      } yield failedPeers.size

    for {
      connections <- ConnectionsCell[F].read
      max         <- RPConfAsk[F].reader(_.clearConnections.maxNumOfConnections)
      cleared     <- if (connections.size > ((max * 2) / 3)) clear(connections) else 0.pure[F]
    } yield cleared
  }

  def findAndConnect[
      F[_]: Capture: Monad: Log: Time: Metrics: NodeDiscovery: ErrorHandler: ConnectionsCell: RPConfAsk](
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
              Log[F].warn(s"Failed to connect to ${peer.toAddress}. Reason: ${error.message}")
            case (peer, Right(_)) =>
              Log[F].info(s"Connected to ${peer.toAddress}.")
          }
    } yield peersAndResonses.filter(_._2.isRight).map(_._1)

  def connect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: ConnectionsCell: RPConfAsk](
      peer: PeerNode,
      timeout: FiniteDuration): F[Unit] =
    for {
      tss      <- Time[F].currentMillis
      peerAddr = peer.toAddress
      _        <- Log[F].debug(s"Connecting to $peerAddr")
      _        <- Metrics[F].incrementCounter("connects")
      _        <- Log[F].info(s"Initialize protocol handshake to $peerAddr")
      local    <- RPConfAsk[F].reader(_.local)
      ph       = protocolHandshake(local)
      phsresp  <- TransportLayer[F].roundTrip(peer, ph, timeout * 2) >>= ErrorHandler[F].fromEither
      _ <- Log[F].debug(
            s"Received protocol handshake response from ${ProtocolHelper.sender(phsresp)}.")
      _   <- ConnectionsCell[F].modify(_.addConn[F](peer))
      tsf <- Time[F].currentMillis
      _   <- Metrics[F].record("connect-time-ms", tsf - tss)
    } yield ()

}
