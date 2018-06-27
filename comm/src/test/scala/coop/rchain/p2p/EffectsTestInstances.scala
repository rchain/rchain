package coop.rchain.p2p

import java.net.SocketAddress
import scala.concurrent.duration.{Duration, MILLISECONDS}

import cats._
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.p2p.effects._
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._
import coop.rchain.comm.protocol.routing._

/** Eagerly evaluated instances to do reasoning about applied effects */
object EffectsTestInstances {

  class LogicalTime[F[_]: Capture] extends Time[F] {
    var clock: Long = 0

    def currentMillis: F[Long] = Capture[F].capture {
      this.clock = clock + 1
      clock
    }

    def nanoTime: F[Long] = Capture[F].capture {
      this.clock = clock + 1
      clock
    }
  }

  class NodeDiscoveryStub[F[_]: Capture]() extends NodeDiscovery[F] {

    var nodes: List[PeerNode] = List.empty[PeerNode]

    def reset(): Unit =
      nodes = List.empty[PeerNode]

    def addNode(node: PeerNode): F[Unit] = Capture[F].capture {
      nodes = node :: nodes
    }

    def peers: F[Seq[PeerNode]] = Capture[F].capture {
      nodes
    }

    def findMorePeers(limit: Int): F[Seq[PeerNode]]                = ???
    def handleCommunications: Protocol => F[CommunicationResponse] = ???
  }

  class TransportLayerStub[F[_]: Capture: Applicative](src: PeerNode) extends TransportLayer[F] {
    type Responses = PeerNode => (Protocol => CommErr[Protocol])
    var reqresp: Option[Responses] = None
    var requests: List[Protocol]   = List.empty[Protocol]

    def setResponses(responses: Responses): Unit =
      reqresp = Some(responses)

    def reset(): Unit = {
      reqresp = None
      requests = List.empty[Protocol]
    }

    def roundTrip(msg: Protocol,
                  remote: PeerNode,
                  timeout: Duration = Duration(500, MILLISECONDS)): F[CommErr[Protocol]] =
      Capture[F].capture {
        requests = requests :+ msg
        reqresp.get.apply(remote).apply(msg)
      }

    def local: F[PeerNode] = src.pure[F]
    def send(msg: Protocol, peer: PeerNode): F[CommErr[Unit]] =
      Capture[F].capture {
        requests = requests :+ msg
        Right(())
      }
    def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] = ???

    def receive(dispatch: Protocol => F[CommunicationResponse]): F[Unit] = ???
  }

  class LogStub[F[_]: Applicative] extends Log[F] {

    var infos: List[String]  = List.empty[String]
    var warns: List[String]  = List.empty[String]
    var errors: List[String] = List.empty[String]

    def reset(): Unit = {
      infos = List.empty[String]
      warns = List.empty[String]
      errors = List.empty[String]
    }
    def debug(msg: String): F[Unit] = ().pure[F]
    def info(msg: String): F[Unit] = {
      infos = infos :+ msg
      ().pure[F]
    }
    def warn(msg: String): F[Unit] = {
      warns = warns :+ msg
      ().pure[F]
    }
    def error(msg: String): F[Unit] = {
      errors = errors :+ msg
      ().pure[F]
    }
  }

}
