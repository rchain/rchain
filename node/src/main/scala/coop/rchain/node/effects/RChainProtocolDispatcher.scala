package coop.rchain.node.effects

import java.net.SocketAddress
import coop.rchain.p2p.effects._
import coop.rchain.comm._
import coop.rchain.p2p.Network.{dispatch => networkDispatch, ErrorHandler, KeysStore}
import coop.rchain.metrics.Metrics
import coop.rchain.catscontrib.Capture
import cats._, cats.data._, cats.implicits._

object RChainProtocolDispatcher {
  def get[
      F[_]: Monad: Capture: Log: Time: Metrics: TransportLayer: NodeDiscovery: Encryption: ErrorHandler: KeysStore: PacketHandler]
    : ProtocolDispatcher[F, SocketAddress] = new ProtocolDispatcher[F, SocketAddress]() {
    def dispatch(extra: SocketAddress, msg: ProtocolMessage): F[Unit] =
      networkDispatch[F](extra, msg)
    def exists: F[Boolean] = true.pure[F]
  }
}
