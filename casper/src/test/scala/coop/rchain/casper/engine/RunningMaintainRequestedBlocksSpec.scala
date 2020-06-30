package coop.rchain.casper.engine

import coop.rchain.catscontrib.ski._
import coop.rchain.casper.protocol._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}
import CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}
import ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime, TransportLayerStub}
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import coop.rchain.metrics.Metrics
import monix.eval.Coeval
import concurrent.duration._
import org.scalatest._

class RunningMaintainRequestedBlocksSpec extends FunSpec with BeforeAndAfterEach with Matchers {

  // This is a test for BlockRetriever
  // TODO move to BlockRetriever tests
  /*
  val hash                    = ByteString.copyFrom("hash", "UTF-8")
  implicit val metrics        = new Metrics.MetricsNOP[Coeval]
  val timeout: FiniteDuration = 240.seconds

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
    log.reset()
    time.reset()
  }

  describe("Running") {
    describe("maintainRequestedBlocks, for every block that was requested") {
      describe("if block request is still within a timeout") {
        it("should keep the request not touch") {
          val requested                = Requested(timestamp = notTimedOut)
          implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
          // when
          Running.maintainRequestedBlocks[Coeval](timeout).apply()
          // then
          val requestedBlocksMapAfter = requestedBlocks.read.apply
          requestedBlocksMapAfter.size should be(1)
        }
      }
      describe("if block was not delivered within given timeout") {
        describe("if waiting list is not empty") {
          it("should request block from first peer on a waiting list") {
            // given
            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
            val requested = Requested(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
            // when
            Running.maintainRequestedBlocks[Coeval](timeout).apply()
            // then
            val (recipient, msg) = transport.getRequest(0)
            toBlockRequest(msg).hash should be(hash)
            recipient shouldBe waitingList(0)
            transport.requests.size shouldBe 1
          }
          it("should move that peer from the waiting list to the requested set") {
            // given
            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
            val requested = Requested(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
            // when
            Running.maintainRequestedBlocks[Coeval](timeout).apply()
            // then
            val Some(requestedAfter) = requestedBlocks.read.apply.get(hash)
            requestedAfter.waitingList shouldBe List(peerNode("waiting2"))
            requestedAfter.peers shouldBe Set(peerNode("peer"), peerNode("waiting1"))
          }
          it("timestamp is reset") {
            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
            val requested = Requested(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
            // when
            Running.maintainRequestedBlocks[Coeval](timeout).apply()
            // then
            val Some(requestedAfter) = requestedBlocks.read.apply.get(hash)
            requestedAfter.timestamp shouldBe time.clock
          }
        }
        describe("if waiting list IS empty") {
          it("should log a warning to the user") {
            // given
            val waitingList = List.empty[PeerNode]
            val requested = Requested(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
            // when
            Running.maintainRequestedBlocks[Coeval](timeout).apply()
            // then
            log.warns should contain(
              s"Could not retrieve requested block ${PrettyPrinter.buildString(hash)} " +
                s"from ${requested.peers.mkString(",")}. Removing the request from the requested blocks list. " +
                s"Casper will have to re-request the block."
            )
            log.warns.size shouldBe 1
          }
          it("should NOT send requests to other peers") {
            // given
            val waitingList = List.empty[PeerNode]
            val requested = Requested(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
            // when
            Running.maintainRequestedBlocks[Coeval](timeout).apply()
            // then
            transport.requests.size should be(0)
          }
          it("should remove the entry from the requested block lists") {
            // given
            val waitingList = List.empty[PeerNode]
            val requested = Requested(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
            // when
            Running.maintainRequestedBlocks[Coeval](timeout).apply()
            // then
            val requestedBlocksMapAfter = requestedBlocks.read.apply
            requestedBlocksMapAfter.size should be(0)
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
  implicit private val log       = new LogStub[Coeval]
  implicit private val time      = new LogicalTime[Coeval]

  private def initRequestedBlocks(
      init: Map[BlockHash, Requested]
  ): RequestedBlocks[Coeval] =
    Cell.unsafe[Coeval, Map[BlockHash, Running.Requested]](init)

  private def toBlockRequest(protocol: Protocol): BlockRequest =
    BlockRequest.from(convert[PacketTypeTag.BlockRequest.type](toPacket(protocol).right.get).get)

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int = 40400): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))

  private def timedOut: Long    = time.clock - (2 * timeout.toMillis)
  private def notTimedOut: Long = time.clock - 1

 */
}
