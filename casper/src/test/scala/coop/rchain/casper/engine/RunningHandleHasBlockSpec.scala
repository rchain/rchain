package coop.rchain.casper.engine

import Running.{Requested, RequestedBlocks}
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}, CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}, ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.TransportLayerStub
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalatest._
import cats._, cats.data._, cats.implicits._

import scala.collection.mutable.{Map => MutableMap}

class RunningHandleHasBlockSpec extends WordSpec with BeforeAndAfterEach with Matchers {

  val hash = ByteString.copyFrom("hash", "UTF-8")
  val hb   = HasBlock(hash)

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  "Running.handleHasBlock" should {
    "not request block if casper contains block" in {
      // given
      implicit val requestedBlocks                     = initRequestedBlocks()
      val casperContains: BlockHash => Coeval[Boolean] = kp(Coeval(true))
      // when
      Running.handleHasBlock[Coeval](null, hb)(casperContains).apply()
      // then
      transport.requests shouldBe empty
    }

    "ignore if requested before for the same peer" in {
      // given
      val sender                   = peerNode("somePeer", 40400)
      val requestedBefore          = Map(hash -> Requested(peers = Set(sender)))
      implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
      val casperContains           = alwaysFalse
      // when
      Running.handleHasBlock[Coeval](sender, hb)(casperContains).apply()
      // then
      transport.requests shouldBe empty
    }

    "store on a waiting list and don't request if requested by different peer" in {
      // given
      val sender                   = peerNode("somePeer", 40400)
      val otherPeer                = peerNode("otherPeer", 40400)
      val requestedBefore          = Map(hash -> Requested(peers = Set(otherPeer)))
      implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
      val casperContains           = alwaysFalse
      // when
      Running.handleHasBlock[Coeval](sender, hb)(casperContains).apply()
      // then
      transport.requests shouldBe empty

      val requested: Requested = requestedBlocks.read.apply().get(hash).get
      requested.peers should be(Set(otherPeer))
      requested.waitingList should be(List(sender))
    }

    "request block and store information about requested block" in {
      // given
      val sender                   = peerNode("somePeer", 40400)
      implicit val requestedBlocks = initRequestedBlocks()
      val casperContains           = alwaysFalse
      // when
      Running.handleHasBlock[Coeval](sender, hb)(casperContains).apply()
      // then
      val (recipient, msg) = transport.getRequest(0)
      // assert requested
      val br: BlockRequest = packetToBlockRequest(toPacket(msg).right.get).get
      br.hash shouldBe hash
      recipient shouldBe sender
      transport.requests.size shouldBe 1
      // assert requested informaton stored
      val requested: Requested = requestedBlocks.read.apply().get(hash).get
      requested.peers should be(Set(sender))
    }
  }

  val local: PeerNode = peerNode("src", 40400)
  val networkId       = "nid"
  val conf            = RPConf(local, networkId, null, null, 0, null)

  implicit private val askConf   = new ConstApplicativeAsk[Coeval, RPConf](conf)
  implicit private val transport = new TransportLayerStub[Coeval]

  private def initRequestedBlocks(
      init: Map[BlockHash, Requested] = Map.empty
  ): RequestedBlocks[Coeval] =
    Cell.unsafe[Coeval, Map[BlockHash, Running.Requested]](init)
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
  private def alwaysFalse: BlockHash => Coeval[Boolean]            = kp(Coeval(false))
}
