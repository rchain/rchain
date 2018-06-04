package coop.rchain.p2p

import org.scalatest._
import com.google.common.io.BaseEncoding
import coop.rchain.comm._, CommError._, NetworkProtocol._
import coop.rchain.p2p.effects._
import cats._
import coop.rchain.catscontrib._, ski._, Encryption._
import coop.rchain.metrics.Metrics

import EffectsTestInstances._

class ConnectToBootstrapSpec
    extends FunSpec
    with Matchers
    with BeforeAndAfterEach
    with AppendedClues {

  val encoder = BaseEncoding.base16().lowerCase()

  val src: PeerNode    = peerNode("src", 30300)
  val remote: PeerNode = peerNode("remote", 30301)
  val srcKeys          = PublicPrivateKeys(encoder.decode("ff00ff00"), encoder.decode("cc00cc00"))
  val remoteKeys       = PublicPrivateKeys(encoder.decode("ee00ee00"), encoder.decode("dd00dd00"))
  val nonce            = encoder.decode("00112233")

  type Effect[A] = CommErrT[Id, A]

  implicit val logEff            = new LogStub[Effect]
  implicit val timeEff           = new LogicalTime[Effect]
  implicit val metricEff         = new Metrics.MetricsNOP[Effect]
  implicit val nodeDiscoveryEff  = new NodeDiscoveryStub[Effect]()
  implicit val transportLayerEff = new TransportLayerStub[Effect](src)
  implicit val encryptionEff     = new EncryptionStub[Effect](srcKeys, nonce)
  implicit val keysStoreEff      = new Kvs.InMemoryKvs[Effect, PeerNode, Key]

  override def beforeEach(): Unit = {
    logEff.reset()
    nodeDiscoveryEff.reset()
    transportLayerEff.reset()
    encryptionEff.reset()
    keysStoreEff.keys.map(_.map(k => keysStoreEff.delete(k)))
  }

  describe("Node when connecting to bootstrap") {
    it("should run attempts before it exits") {
      // given
      transportLayerEff.setResponses(kp(failEverything))
      // when
      val _ = Network.connectToBootstrap[Effect](remote.toAddress, maxNumOfAttempts = 5)
      // then
      logEff.warns should equal(
        List(
          "Failed to connect to bootstrap (attempt 1 / 5)",
          "Failed to connect to bootstrap (attempt 2 / 5)",
          "Failed to connect to bootstrap (attempt 3 / 5)",
          "Failed to connect to bootstrap (attempt 4 / 5)",
          "Failed to connect to bootstrap (attempt 5 / 5)"
        ))
    }

    it("should log on ERROR and return error when failed connecting") {
      // given
      transportLayerEff.setResponses(kp(failEverything))
      // when
      val result = Network.connectToBootstrap[Effect](remote.toAddress, maxNumOfAttempts = 5)
      // then
      logEff.errors should equal(List("Failed to connect to bootstrap node, exiting..."))
      result.value should equal(Left(couldNotConnectToBootstrap))
    }

    it("should connect smoothly if there are no issues.") {
      // given
      transportLayerEff.setResponses(kp(generateResponses(fstPhase, sndPhaseSucc)))
      // when
      val result = Network.connectToBootstrap[Effect](remote.toAddress, maxNumOfAttempts = 5)
      // then
      logEff.infos should contain(s"Bootstrapping from $remote.")
      logEff.infos should contain(s"Connected $remote.")
      result.value should equal(Right(()))
    }
  }

  private val fstPhase: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]] = {
    case hs @ EncryptionHandshakeMessage(_, _) =>
      hs.response[Effect](remote, remoteKeys).value.right.get
  }

  private val failEverything = kp(Left[CommError, ProtocolMessage](unknownProtocol("unknown")))

  private val sndPhaseSucc: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]] = {
    case hs @ FrameMessage(_, _) =>
      Right(
        FrameMessage(frameResponse(remote, hs.header.get, Array.empty[Byte], Array.empty[Byte]), 1))
  }

  private def generateResponses(
      fstPhase: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]],
      sndPhase: PartialFunction[ProtocolMessage, CommErr[ProtocolMessage]])
    : ProtocolMessage => CommErr[ProtocolMessage] =
    fstPhase orElse sndPhase

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
