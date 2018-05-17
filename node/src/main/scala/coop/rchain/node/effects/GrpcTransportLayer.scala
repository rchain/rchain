package coop.rchain.node.effects

import java.net.SocketAddress
import coop.rchain.comm._, CommError._
import coop.rchain.p2p.effects._
import coop.rchain.metrics.Metrics

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._, ski._, TaskContrib._

object GrpcTransportLayer {
  def get[F[_]: Monad: Capture: Metrics]: TransportLayer[F] =
    new TransportLayer[F] {
      import scala.concurrent.duration._

      def roundTrip(msg: ProtocolMessage,
                    remote: ProtocolNode,
                    timeout: Duration): F[CommErr[ProtocolMessage]] = ???

      def local: F[ProtocolNode] = ???

      def commSend(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] = ???

      def broadcast(msg: ProtocolMessage): F[Seq[CommErr[Unit]]] = ???

      def receive: F[Option[ProtocolMessage]] = ???
    }
}
