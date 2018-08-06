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
  type Connections           = Set[Connection]
  type ConnectionsCell[F[_]] = Cell[F, Connections]
  object ConnectionsCell {
    def apply[F[_]](implicit ev: ConnectionsCell[F]): ConnectionsCell[F] = ev
  }
  object Connections {
    def empty: Connections = Set.empty[Connection]
    implicit class ConnectionsOps(connections: Connections) {
      def addConn[F[_]: Applicative](connection: Connection): F[Connections] =
        (connections + connection).pure[F]
    }
  }
  import Connections._

  type RPConfAsk[F[_]] = ApplicativeAsk[F, RPConf]
  object RPConfAsk {
    def apply[F[_]](implicit ev: ApplicativeAsk[F, RPConf]): ApplicativeAsk[F, RPConf] = ev
  }

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def clearConnections[F[_]: Capture: Monad: ConnectionsCell: RPConfAsk]: F[Int] = {

    def clear: F[Int] = 0.pure[F]

    for {
      connections <- ConnectionsCell[F].read
      max         <- RPConfAsk[F].reader(_.clearConnections.maxNumOfConnections)
      cleared     <- if (connections.size > ((max * 2) / 3)) clear else 0.pure[F]
    } yield cleared
  }

  def findAndConnect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: ConnectionsCell](
      defaultTimeout: FiniteDuration): Int => F[Int] =
    (lastCount: Int) =>
      for {
        _         <- IOUtil.sleep[F](5000L)
        peers     <- NodeDiscovery[F].findMorePeers(10).map(_.toList)
        responses <- peers.traverse(connect[F](_, defaultTimeout).attempt)
        _ <- peers.zip(responses).traverse {
              case (peer, Left(error)) =>
                Log[F].warn(s"Failed to connect to ${peer.toAddress}. Reason: ${error.message}")
              case (_, Right(_)) => ().pure[F]
            }
        thisCount <- NodeDiscovery[F].peers.map(_.size)
        _         <- (thisCount != lastCount).fold(Log[F].info(s"Peers: $thisCount."), ().pure[F])
      } yield thisCount

  def connectToBootstrap[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: ConnectionsCell](
      bootstrap: PeerNode,
      maxNumOfAttempts: Int = 5,
      defaultTimeout: FiniteDuration): F[Unit] = {

    def connectAttempt(attempt: Int, timeout: FiniteDuration, bootstrapAddr: PeerNode): F[Unit] =
      if (attempt > maxNumOfAttempts) for {
        _ <- Log[F].error("Failed to connect to bootstrap node, exiting...")
        _ <- ErrorHandler[F].raiseError[Unit](couldNotConnectToBootstrap)
      } yield ()
      else
        for {
          res <- connect[F](bootstrapAddr, timeout).attempt
          _ <- res match {
                case Left(error) =>
                  val msg =
                    "Failed to connect to bootstrap " +
                      s"(attempt $attempt / $maxNumOfAttempts). Reason: ${error.message}"
                  Log[F].warn(msg) *> connectAttempt(attempt + 1,
                                                     timeout + defaultTimeout,
                                                     bootstrapAddr)
                case Right(_) => ().pure[F]
              }
        } yield ()

    for {
      _ <- Log[F].info(s"Bootstrapping from ${bootstrap.toAddress}.")
      _ <- connectAttempt(attempt = 1, defaultTimeout, bootstrap)
      _ <- Log[F].info(s"Connected ${bootstrap.toAddress}.")
    } yield ()
  }

  def connect[
      F[_]: Capture: Monad: Log: Time: Metrics: TransportLayer: NodeDiscovery: ErrorHandler: ConnectionsCell](
      peer: PeerNode,
      timeout: FiniteDuration): F[Unit] =
    for {
      tss      <- Time[F].currentMillis
      peerAddr = peer.toAddress
      _        <- Log[F].debug(s"Connecting to $peerAddr")
      _        <- Metrics[F].incrementCounter("connects")
      _        <- Log[F].info(s"Initialize protocol handshake to $peerAddr")
      local    <- TransportLayer[F].local
      ph       = protocolHandshake(local)
      phsresp  <- TransportLayer[F].roundTrip(peer, ph, timeout) >>= ErrorHandler[F].fromEither
      _ <- Log[F].debug(
            s"Received protocol handshake response from ${ProtocolHelper.sender(phsresp)}.")
      _   <- NodeDiscovery[F].addNode(peer)
      _   <- ConnectionsCell[F].modify(_.addConn[F](peer))
      tsf <- Time[F].currentMillis
      _   <- Metrics[F].record("connect-time-ms", tsf - tss)
    } yield ()

}
