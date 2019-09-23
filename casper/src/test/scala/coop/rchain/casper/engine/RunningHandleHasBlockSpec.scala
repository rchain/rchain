package coop.rchain.casper.engine

import Running.{Requested, RequestedBlocks}
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}, CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}, ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime, TransportLayerStub}
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalatest._
import cats._, cats.data._, cats.implicits._

import scala.collection.mutable.{Map => MutableMap}

class RunningHandleHasBlockSpec extends FunSpec with BeforeAndAfterEach with Matchers {

  implicit private def log = new LogStub[Coeval]

  val hash = ByteString.copyFrom("hash", "UTF-8")
  val hb   = HasBlock(hash)

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
    time.reset()
  }

  describe("Running") {
    describe("handleHasBlock") {
      describe("if casper contains block with given hash already") {
        it("should not request block") {
          // given
          implicit val requestedBlocks                     = initRequestedBlocks()
          val casperContains: BlockHash => Coeval[Boolean] = kp(Coeval(true))
          // when
          Running.handleHasBlock[Coeval](null, hb)(casperContains).apply()
          // then
          transport.requests shouldBe empty
        }
      }
      describe("if casper does NOT contain block with given hash") {
        describe("if there is already an entry in the requested blocks") {
          it("should ignore if peer on the requested peers list") {
            // given
            val sender = peerNode("somePeer", 40400)
            val requestedBefore =
              Map(hash -> Requested(timestamp = System.currentTimeMillis, peers = Set(sender)))
            implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
            val casperContains           = alwaysFalse
            // when
            Running.handleHasBlock[Coeval](sender, hb)(casperContains).apply()
            // then
            transport.requests shouldBe empty
          }
          it("should store on a waiting list and don't request if requested by different peer") {
            // given
            val sender    = peerNode("somePeer", 40400)
            val otherPeer = peerNode("otherPeer", 40400)
            val requestedBefore =
              Map(hash -> Requested(timestamp = System.currentTimeMillis, peers = Set(otherPeer)))
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
          it(
            "should request block and add peer to peers list if peers list is empty"
          ) {
            // given
            val sender = peerNode("somePeer", 40400)
            val requestedBefore =
              Map(
                hash -> Requested(
                  timestamp = System.currentTimeMillis,
                  peers = Set.empty,
                  waitingList = List.empty
                )
              )
            implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
            val casperContains           = alwaysFalse
            // when
            Running.handleHasBlock[Coeval](sender, hb)(casperContains).apply()
            // then
            val (recipient, msg) = transport.getRequest(0)
            // assert requested
            val br: BlockRequest =
              BlockRequest.from(packetToBlockRequest(toPacket(msg).right.get).get)
            br.hash shouldBe hash
            recipient shouldBe sender
            transport.requests.size shouldBe 1
            // assert requested informaton stored
            val requested: Requested = requestedBlocks.read.apply().get(hash).get
            requested.peers should be(Set(sender))
          }
        }
        describe("if there is no yet an an entry in the requested blocks") {
          it("should request block and store information about requested block") {
            // given
            val sender                   = peerNode("somePeer", 40400)
            implicit val requestedBlocks = initRequestedBlocks()
            val casperContains           = alwaysFalse
            // when
            Running.handleHasBlock[Coeval](sender, hb)(casperContains).apply()
            // then
            val (recipient, msg) = transport.getRequest(0)
            // assert requested
            val br: BlockRequest =
              BlockRequest.from(packetToBlockRequest(toPacket(msg).right.get).get)
            br.hash shouldBe hash
            recipient shouldBe sender
            transport.requests.size shouldBe 1
            // assert requested informaton stored
            val requested: Requested = requestedBlocks.read.apply().get(hash).get
            requested.peers should be(Set(sender))
          }

        }
      }

    }
  }

  val local: PeerNode = peerNode("src", 40400)
  val networkId       = "nid"
  val conf            = RPConf(local, networkId, null, null, 0, null)

  implicit private val askConf   = new ConstApplicativeAsk[Coeval, RPConf](conf)
  implicit private val transport = new TransportLayerStub[Coeval]
  implicit private val time      = new LogicalTime[Coeval]

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
