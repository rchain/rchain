package coop.rchain.p2p

import java.net.SocketAddress
import scala.concurrent.duration.{Duration, MILLISECONDS}

import cats._
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.comm.CommError._
import coop.rchain.comm._
import coop.rchain.p2p.effects._

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

    def findMorePeers(limit: Int): F[Seq[PeerNode]]                       = ???
    def handleCommunications: ProtocolMessage => F[CommunicationResponse] = ???
  }

  class TransportLayerStub[F[_]: Capture: Applicative](src: PeerNode) extends TransportLayer[F] {
    type Responses = PeerNode => (ProtocolMessage => CommErr[ProtocolMessage])
    var reqresp: Option[Responses]      = None
    var requests: List[ProtocolMessage] = List.empty[ProtocolMessage]

    def setResponses(responses: Responses): Unit =
      reqresp = Some(responses)

    def reset(): Unit = {
      reqresp = None
      requests = List.empty[ProtocolMessage]
    }

    def roundTrip(msg: ProtocolMessage,
                  remote: PeerNode,
                  timeout: Duration = Duration(500, MILLISECONDS)): F[CommErr[ProtocolMessage]] =
      Capture[F].capture {
        requests = requests :+ msg
        reqresp.get.apply(remote).apply(msg)
      }

    def local: F[PeerNode] = src.pure[F]
    def send(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] =
      Capture[F].capture {
        requests = requests :+ msg
        Right(())
      }
    def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): F[Seq[CommErr[Unit]]] = ???

    def receive(dispatch: ProtocolMessage => F[CommunicationResponse]): F[Unit] = ???
  }

  import Encryption._

  class EncryptionStub[F[_]: Applicative](keys: PublicPrivateKeys, nonce: Nonce)
      extends Encryption[F] {

    var encryptions: List[(Key, Key, Nonce, Array[Byte])] = Nil
    var decryptions: List[(Key, Key, Nonce, Array[Byte])] = Nil

    def reset(): Unit = {
      encryptions = Nil
      decryptions = Nil
    }

    def fetchKeys: F[PublicPrivateKeys] = keys.pure[F]
    def generateNonce: F[Nonce]         = nonce.pure[F]
    def encrypt(pub: Key, sec: Key, nonce: Nonce, message: Array[Byte]): F[Array[Byte]] = {
      encryptions = encryptions :+ (pub, sec, nonce, message)
      message.pure[F]
    }

    def decrypt(pub: Key, sec: Key, nonce: Nonce, cipher: Array[Byte]): F[Array[Byte]] = {
      decryptions = decryptions :+ (pub, sec, nonce, cipher)
      cipher.pure[F]
    }
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
