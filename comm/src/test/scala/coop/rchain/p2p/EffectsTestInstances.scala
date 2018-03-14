package coop.rchain.p2p

import coop.rchain.comm._, CommError._
import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.catscontrib._
import cats._, cats.data._, cats.implicits._

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

  class CommunicationStub[F[_]: Capture: Applicative](src: ProtocolNode) extends Communication[F] {

    type Responses = ProtocolNode => (ProtocolMessage => CommErr[ProtocolMessage])
    var reqresp: Option[Responses]      = None
    var nodes: List[PeerNode]           = List.empty[PeerNode]
    var requests: List[ProtocolMessage] = List.empty[ProtocolMessage]

    def setResponses(responses: Responses): Unit =
      reqresp = Some(responses)

    def reset: Unit = {
      reqresp = None
      nodes = List.empty[PeerNode]
      requests = List.empty[ProtocolMessage]
    }

    def roundTrip(msg: ProtocolMessage,
                  remote: ProtocolNode,
                  timeout: Duration = Duration(500, MILLISECONDS)): F[CommErr[ProtocolMessage]] =
      Capture[F].capture {
        requests = requests :+ msg
        reqresp.get.apply(remote).apply(msg)
      }

    def local: F[ProtocolNode]                                      = src.pure[F]
    def commSend(data: Seq[Byte], peer: PeerNode): F[CommErr[Unit]] = ???
    def addNode(node: PeerNode): F[Unit] = Capture[F].capture {
      nodes = node :: nodes
    }
    def broadcast(msg: ProtocolMessage): F[Seq[CommErr[Unit]]] = ???
    def findMorePeers(limit: Int): F[Seq[PeerNode]]            = ???
    def countPeers: F[Int]                                     = ???
    def receiver: F[Unit]                                      = ???
  }

  import Encryption._

  class EncryptionStub[F[_]: Applicative](keys: PublicPrivateKeys, nonce: Nonce)
      extends Encryption[F] {
    def fetchKeys: F[PublicPrivateKeys] = keys.pure[F]
    def generateNonce: F[Nonce]         = nonce.pure[F]
  }

}
