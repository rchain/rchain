package coop.rchain.comm.connect

import org.scalatest._
import coop.rchain.comm.protocol.rchain._
import com.google.common.io.BaseEncoding
import coop.rchain.comm._, CommError._
import coop.rchain.p2p.effects._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import coop.rchain.metrics.Metrics
import coop.rchain.comm.transport._, CommMessages._
import coop.rchain.comm.discovery._
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared._
import scala.concurrent.duration.{Duration, MILLISECONDS}

class ConnectSpec extends FunSpec with Matchers with BeforeAndAfterEach with AppendedClues {

  val defaultTimeout: Duration = Duration(1, MILLISECONDS)
  val src: PeerNode            = peerNode("src", 30300)
  val remote: PeerNode         = peerNode("remote", 30301)

  type Effect[A] = CommErrT[Id, A]

  implicit val logEff            = new Log.NOPLog[Effect]
  implicit val timeEff           = new LogicalTime[Effect]
  implicit val metricEff         = new Metrics.MetricsNOP[Effect]
  implicit val nodeDiscoveryEff  = new NodeDiscoveryStub[Effect]()
  implicit val transportLayerEff = new TransportLayerStub[Effect](src)
  implicit val packetHandler     = new PacketHandler.NOPPacketHandler[Effect]

  override def beforeEach(): Unit = {
    nodeDiscoveryEff.reset()
    transportLayerEff.reset()
  }

  describe("Node") {
    describe("when connecting to other remote node") {
      it("should send ProtocolHandshake") {
        // given
        transportLayerEff.setResponses(kp(alwaysSuccess))
        // when
        Connect.connect[Effect](remote, defaultTimeout)
        // then
        transportLayerEff.requests.size should be(1)
        val ProtocolHandshakeMessage(_) = transportLayerEff.requests(0)
      }
      it("should then add remote node to communication layer") {
        // given
        transportLayerEff.setResponses(kp(alwaysSuccess))
        // when
        Connect.connect[Effect](remote, defaultTimeout)
        // then
        nodeDiscoveryEff.nodes should not be empty
      }
    }

    describe("when reciving encrypted ProtocolHandshake") {
      it("should send protocol handshake response back to the remote")(pending)
      it("should add node once protocol handshake response is sent")(pending)
      it("should not respond if message can not be decrypted")(pending)
      it("should not respond if it does not contain remotes public key")(pending)
    }

  }

  def alwaysSuccess: ProtocolMessage => CommErr[ProtocolMessage] =
    kp(Right(ProtocolHandshakeResponseMessage(protocolHandshake(src))))

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
