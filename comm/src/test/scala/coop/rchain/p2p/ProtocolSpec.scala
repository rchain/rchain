package coop.rchain.p2p

import org.scalatest._
import coop.rchain.comm.protocol.rchain._
import com.google.common.io.BaseEncoding
import coop.rchain.comm._, CommError._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, Encryption._

import EffectsTestInstances._

class ProtocolSpec extends FunSpec with Matchers with BeforeAndAfterEach {

  val encoder = BaseEncoding.base16().lowerCase()

  val src: ProtocolNode    = protocolNode("src", 30300)
  val remote: ProtocolNode = protocolNode("remote", 30301)
  val srcKeys              = PublicPrivateKeys(encoder.decode("ff00ff00"), encoder.decode("cc00cc00"))
  val remoteKeys           = PublicPrivateKeys(encoder.decode("ee00ee00"), encoder.decode("dd00dd00"))
  val nonce                = Array.empty[Byte]

  type Effect[A] = CommErrT[Id, A]

  implicit val logEff           = new Log.NOPLog[Effect]
  implicit val timeEff          = new LogicalTime[Effect]
  implicit val metricEff        = new Metrics.MetricsNOP[Effect]
  implicit val communicationEff = new CommunicationStub[Effect](src)
  implicit val encryptionEff    = new EncryptionStub[Effect](srcKeys, nonce)
  implicit val keysStoreEff     = new Kvs.InMemoryKvs[Effect, PeerNode, Key]

  override def beforeEach(): Unit = {
    communicationEff.reset()
    keysStoreEff.keys.map(_.map(k => keysStoreEff.delete(k)))
  }

  describe("Node") {
    describe("when connecting to other remote node") {
      describe(" to which he was not connect in the past") {

        it("should first send EncryptionHanshakeMessage with its public key") {
          // given
          communicationEff.setResponses(kp(generateResponses(fstPhase, sndPhase)))
          // when
          Network.connect[Effect](remote)
          // then
          val EncryptionHandshakeMessage(proto, _) = communicationEff.requests(0)
          val Right(EncryptionHandshake(pk))       = NetworkProtocol.toEncryptionHandshake(proto)
          pk.toByteArray should equal(srcKeys.pub)
        }

        it("should then store remote's public key") {
          // given
          communicationEff.setResponses(kp(generateResponses(fstPhase, sndPhase)))
          // when
          Network.connect[Effect](remote)
          // then
          value(keysStoreEff.get(remote)).map(_.toList) should equal(Some(remoteKeys.pub.toList))
        }

        it("should then send encrypted ProtocolMessage")(pending)

      }
    }

    describe("when receiving encryption handshake from remote node") {
      it("should send back its public key in response") {
        // given
        val receivedMessage =
          EncryptionHandshakeMessage(NetworkProtocol.encryptionHandshake(src, remoteKeys), 1)
        // when
        Network.handleEncryptionHandshake[Effect](remote, receivedMessage)
        // then
        val EncryptionHandshakeResponseMessage(proto, _) = communicationEff.requests(0)
        val Right(EncryptionHandshakeResponse(pk)) =
          NetworkProtocol.toEncryptionHandshakeResponse(proto)
        pk.toByteArray should equal(srcKeys.pub)
      }

      it("should store remote's public key") {
        // given
        val receivedMessage =
          EncryptionHandshakeMessage(NetworkProtocol.encryptionHandshake(src, remoteKeys), 1)
        // when
        Network.handleEncryptionHandshake[Effect](remote, receivedMessage)
        // then
        value(keysStoreEff.get(remote)).map(_.toList) should equal(Some(remoteKeys.pub.toList))
      }

      it("should not store remote's public key if sending response failed with error")(pending)
    }
  }

  private def value[A](ea: Effect[A]): A = ea.value.right.get

  private val fstPhase: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]] = {
    case hs @ EncryptionHandshakeMessage(_, _) =>
      hs.response[Effect](remote, remoteKeys).value.right.get
  }

  private val sndPhase: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]] = {
    case hs @ ProtocolHandshakeMessage(_, _) => hs.response(remote)
  }

  private def generateResponses(
      fstPhase: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]],
      sndPhase: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]])
    : ProtocolMessage => CommErr[ProtocolMessage] =
    fstPhase orElse sndPhase

  private val roundTripNOP =
    kp2[ProtocolMessage, ProtocolNode, CommErr[ProtocolMessage]](Left(unknownProtocol("unknown")))
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def protocolNode(name: String, port: Int): ProtocolNode =
    ProtocolNode(new PeerNode(NodeIdentifier(name.getBytes), endpoint(port)), roundTripNOP)
}
