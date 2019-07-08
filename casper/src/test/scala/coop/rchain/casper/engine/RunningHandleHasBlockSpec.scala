package coop.rchain.casper.engine

import coop.rchain.catscontrib.ski._
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}, CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}, ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.{Request, TransportLayerStub}
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalatest._
import cats._, cats.data._, cats.implicits._

class RunningHandleHasBlockSpec extends WordSpec with BeforeAndAfterEach with Matchers {

  val hash            = ByteString.copyFrom("hash", "UTF-8")
  val hb              = HasBlock(hash)
  val local: PeerNode = peerNode("src", 40400)
  val networkId       = "nid"
  val conf            = RPConf(local, networkId, null, null, 0, null)

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  "Running.handleHasBlock" should {
    "not request block if casper contains block" in {
      // given
      val casperContains: BlockHash => Coeval[Boolean] = kp(Coeval(true))
      // when
      Running.handleHasBlock[Coeval](null, hb)(casperContains).apply()
      // then
      transport.requests shouldBe empty
    }

    "request block" in {
      // given
      val sender                                       = peerNode("somePeer", 40400)
      val casperContains: BlockHash => Coeval[Boolean] = kp(Coeval(false))
      // when
      Running.handleHasBlock[Coeval](sender, hb)(casperContains).apply()
      // then
      val Request(recipient, msg) = transport.requests(0)
      val br: BlockRequest        = packetToBlockRequest(toPacket(msg).right.get).get
      br.hash shouldBe hash
      recipient shouldBe sender
      transport.requests.size shouldBe 1
    }
  }

  implicit private val askConf   = new ConstApplicativeAsk[Coeval, RPConf](conf)
  implicit private val transport = new TransportLayerStub[Coeval]

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))

}
