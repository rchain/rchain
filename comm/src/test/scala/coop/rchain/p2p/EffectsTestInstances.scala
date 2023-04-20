package coop.rchain.p2p

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.comm.discovery._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp._
import coop.rchain.comm.transport._
import coop.rchain.shared.Log.NOPLog
import coop.rchain.shared._

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

/** Eagerly evaluated instances to do reasoning about applied effects */
object EffectsTestInstances {

  val networkId = "test"

  class LogicalTime[F[_]: Sync] {
    var clock: Long = 0

    def currentMillis: F[Long] = Sync[F].delay {
      this.clock = clock + 1
      clock
    }

    def nanoTime: F[Long] = Sync[F].delay {
      this.clock = clock + 1
      clock
    }

    def sleep(duration: FiniteDuration): F[Unit] = Sync[F].delay(())

    def reset(): Unit = this.clock = 0
  }

  class NodeDiscoveryStub[F[_]: Sync]() extends NodeDiscovery[F] {

    var nodes: List[PeerNode] = List.empty[PeerNode]
    def reset(): Unit =
      nodes = List.empty[PeerNode]
    def peers: F[Seq[PeerNode]] = Sync[F].delay {
      nodes
    }
    def discover: F[Unit]                                          = ???
    def handleCommunications: Protocol => F[CommunicationResponse] = ???
  }

  def createRPConfAsk[F[_]: Applicative](
      local: PeerNode,
      defaultTimeout: FiniteDuration = FiniteDuration(1, MILLISECONDS),
      clearConnections: ClearConnectionsConf = ClearConnectionsConf(1)
  ) =
    new ConstApplicativeAsk[F, RPConf](
      RPConf(local, networkId, Some(local), defaultTimeout, 20, clearConnections)
    )

  class TransportLayerStub[F[_]: Sync] extends TransportLayer[F] {
    import java.util.concurrent.LinkedBlockingQueue

    case class Request(peer: PeerNode, msg: Protocol)
    type Responses = PeerNode => Protocol => CommErr[Unit]
    var reqresp: Option[Responses] = None
    var requests: List[Request]    = List.empty[Request]
    // TODO: Replacement for SyncVar (legacy code should be replaced with Mockito mocks)
    val lock = new LinkedBlockingQueue[Unit](1)
    lock.put(())

    def setResponses(responses: Responses): Unit =
      reqresp = Some(responses)

    def atomically[A](operation: => A): A = {
      lock.take()
      val result = operation
      lock.put(())
      result
    }

    def reset(): Unit =
      atomically({
        reqresp = None
        requests = List.empty[Request]
      })

    def getRequest(i: Int): (PeerNode, Protocol) = (requests(i).peer, requests(i).msg)

    override def send(peer: PeerNode, msg: Protocol): F[CommErr[Unit]] =
      Sync[F].delay {
        atomically(this.requests = this.requests :+ Request(peer, msg))
        reqresp.get.apply(peer).apply(msg)
      }

    override def broadcast(peers: Seq[PeerNode], msg: Protocol): F[Seq[CommErr[Unit]]] =
      Sync[F].delay {
        atomically(this.requests = this.requests ++ peers.map(peer => Request(peer, msg)))
        peers.map(_ => Right(()))
      }

    override def stream(peers: Seq[PeerNode], blob: Blob): F[Unit] =
      broadcast(peers, ProtocolHelper.protocol(blob.sender, networkId).withPacket(blob.packet)).void
  }

  class LogStub[F[_]: Sync](delegate: Log[F]) extends Log[F] {

    def this() = this(new NOPLog[F]())

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
    def isTraceEnabled(implicit ev: LogSource): F[Boolean]     = false.pure[F]
    def trace(msg: => String)(implicit ev: LogSource): F[Unit] = ().pure[F]
    def debug(msg: => String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(this.debugs = this.debugs :+ msg) >> delegate.debug(msg)
    def info(msg: => String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(this.infos = this.infos :+ msg) >> delegate.info(msg)
    def warn(msg: => String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(this.warns = this.warns :+ msg) >> delegate.warn(msg)
    def warn(msg: => String, cause: scala.Throwable)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(this.warns = this.warns :+ msg) >> delegate.warn(msg, cause)
    def error(msg: => String)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(this.errors = this.errors :+ msg) >> delegate.error(msg)
    def error(msg: => String, cause: scala.Throwable)(implicit ev: LogSource): F[Unit] =
      Sync[F].delay(this.errors = this.errors :+ msg) >> delegate.error(msg, cause)
  }

}
