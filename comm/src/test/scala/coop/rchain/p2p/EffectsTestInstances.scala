package coop.rchain.p2p

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

import cats._
import cats.implicits._

import coop.rchain.comm.rp._
import coop.rchain.catscontrib._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
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

    def sleep(duration: FiniteDuration): F[Unit] = Capture[F].capture(())

    def reset(): Unit = this.clock = 0
  }

  class NodeDiscoveryStub[F[_]: Capture]() extends NodeDiscovery[F] {

    var nodes: List[PeerNode] = List.empty[PeerNode]
    def reset(): Unit =
      nodes = List.empty[PeerNode]
    def peers: F[Seq[PeerNode]] = Capture[F].capture {
      nodes
    }
    def discover: F[Unit]                                          = ???
    def handleCommunications: Protocol => F[CommunicationResponse] = ???
  }

  def createRPConfAsk[F[_]: Applicative](
      local: PeerNode,
      defaultTimeout: FiniteDuration = FiniteDuration(1, MILLISECONDS),
      clearConnections: ClearConnetionsConf = ClearConnetionsConf(1, 1)
  ) =
    new ConstApplicativeAsk[F, RPConf](
      RPConf(local, Some(local), defaultTimeout, clearConnections)
    )

  class TransportLayerStub[F[_]: Capture: Applicative] extends TransportLayer[F] {
    case class Request(peer: PeerNode, msg: Protocol)
    type Responses = PeerNode => Protocol => CommErr[Protocol]
    var reqresp: Option[Responses]  = None
    var requests: List[Request]     = List.empty[Request]
    var disconnects: List[PeerNode] = List.empty[PeerNode]

    def setResponses(responses: Responses): Unit =
      reqresp = Some(responses)

    def reset(): Unit = {
      reqresp = None
      requests = List.empty[Request]
      disconnects = List.empty[PeerNode]
    }

    def roundTrip(peer: PeerNode, msg: Protocol, timeout: FiniteDuration): F[CommErr[Protocol]] =
      Capture[F].capture {
        requests = requests :+ Request(peer, msg)
        reqresp.get.apply(peer).apply(msg)
      }

    def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]] =
      Capture[F].capture {
        requests = requests :+ Request(peer, msg)
        Right(())
      }

    def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]] = Capture[F].capture {
      requests = requests ++ peers.map(peer => Request(peer, msg))
      peers.map(_ => Right(()))
    }

    def stream(peers: Seq[PeerNode], blob: Blob): F[Unit] =
      broadcast(peers, ProtocolHelper.protocol(blob.sender).withPacket(blob.packet)).void

    def receive(
        dispatch: Protocol => F[CommunicationResponse],
        handleStreamed: Blob => F[Unit]
    ): F[Unit] = ???

    def disconnect(peer: PeerNode): F[Unit] =
      Capture[F].capture {
        disconnects = disconnects :+ peer
      }

    def shutdown(msg: Protocol): F[Unit] = ???
  }

  class LogStub[F[_]: Applicative] extends Log[F] {

    var debugs: List[String] = List.empty[String]
    var infos: List[String]  = List.empty[String]
    var warns: List[String]  = List.empty[String]
    var errors: List[String] = List.empty[String]

    def reset(): Unit = {
      debugs = List.empty[String]
      infos = List.empty[String]
      warns = List.empty[String]
      errors = List.empty[String]
    }
    def isTraceEnabled(implicit ev: LogSource): F[Boolean]  = false.pure[F]
    def trace(msg: String)(implicit ev: LogSource): F[Unit] = ().pure[F]
    def debug(msg: String)(implicit ev: LogSource): F[Unit] = {
      debugs = debugs :+ msg
      ().pure[F]
    }
    def info(msg: String)(implicit ev: LogSource): F[Unit] = {
      infos = infos :+ msg
      ().pure[F]
    }
    def warn(msg: String)(implicit ev: LogSource): F[Unit] = {
      warns = warns :+ msg
      ().pure[F]
    }
    def error(msg: String)(implicit ev: LogSource): F[Unit] = {
      errors = errors :+ msg
      ().pure[F]
    }
    def error(msg: String, cause: scala.Throwable)(implicit ev: LogSource): F[Unit] = {
      errors = errors :+ msg
      ().pure[F]
    }
  }

}
