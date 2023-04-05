package coop.rchain.casper.sync

import cats.effect.IO
import com.google.protobuf.ByteString
import coop.rchain.casper.blocks.BlockRetriever
import coop.rchain.casper.blocks.BlockRetriever.{RequestState, RequestedBlocks}
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.comm.rp.ProtocolHelper.toPacket
import coop.rchain.comm.rp.RPConf
import coop.rchain.comm.{Endpoint, NodeIdentifier, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances.{createRPConfAsk, LogStub, TransportLayerStub}
import coop.rchain.shared.Log
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import cats.effect.Ref

class BlockRetrieverRequestAllSpec extends AnyFunSpec with BeforeAndAfterEach with Matchers {

  object testReason extends BlockRetriever.AdmitHashReason

  val hash                    = ByteString.copyFrom("newHash", "utf-8")
  val timeout: FiniteDuration = 240.seconds
  val local: PeerNode         = peerNode("src", 40400)
  import coop.rchain.shared.RChainScheduler._

  implicit val log: Log[IO] = new LogStub
  implicit val metrics      = new Metrics.MetricsNOP[IO]
  implicit val currentRequests: RequestedBlocks[IO] =
    Ref.unsafe[IO, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])
  implicit val connectionsCell: ConnectionsCell[IO] =
    Ref.unsafe[IO, Connections](List(local))
  implicit val transportLayer = new TransportLayerStub[IO]
  implicit val rpConf         = createRPConfAsk[IO](local)
  implicit val commUtil       = CommUtil.of[IO]
  implicit val blockRetriever = BlockRetriever.of[IO]

  val networkId = "nid"
  val conf      = RPConf(local, networkId, null, null, 0, null)

  private def toBlockRequest(protocol: Protocol): BlockRequest =
    BlockRequest.from(convert[PacketTypeTag.BlockRequest.type](toPacket(protocol).right.get).get)
  private def toHasBlockRequest(protocol: Protocol): HasBlockRequest =
    HasBlockRequest.from(
      convert[PacketTypeTag.HasBlockRequest.type](toPacket(protocol).right.get).get
    )

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int = 40400): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))

  private def timedOut: Long = -2 * timeout.toMillis
//  private def notTimedOut: Long = -1

  override def beforeEach(): Unit = {
    transportLayer.reset()
    transportLayer.setResponses(alwaysSuccess)
    currentRequests.set(Map.empty).unsafeRunSync
  }

  describe("Running") {
    describe("maintainRequestedBlocks, for every block that was requested") {
      describe("if block request is still within a timeout") {
        it("should keep the request not touch") {
          val requested =
            RequestState(timestamp = timer.clock.realTime(TimeUnit.MILLISECONDS).unsafeRunSync)
          currentRequests.set(Map(hash -> requested)).unsafeRunSync
          // when
          blockRetriever.requestAll(timeout).unsafeRunSync
          // then
          val requestedBlocksMapAfter = currentRequests.get.unsafeRunSync
          requestedBlocksMapAfter.size should be(1)
        }
      }
      describe("if block was not delivered within given timeout") {
        describe("if waiting list is not empty") {
          it("should request block from first peer on a waiting list") {
            // given
            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
            val requested = RequestState(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            currentRequests.set(Map(hash -> requested)).unsafeRunSync
            // when
            blockRetriever.requestAll(timeout).unsafeRunSync
            // then
            val (recipient, msg) = transportLayer.getRequest(0)
            toBlockRequest(msg).hash should be(hash)
            recipient shouldBe waitingList.head
            transportLayer.requests.size shouldBe 1
          }
          it("should move that peer from the waiting list to the requested set") {
            // given
            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
            val requested = RequestState(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            currentRequests.set(Map(hash -> requested)).unsafeRunSync
            // when
            blockRetriever.requestAll(timeout).unsafeRunSync
            // then
            val Some(requestedAfter) = currentRequests.get.unsafeRunSync.get(hash)
            requestedAfter.waitingList shouldBe List(peerNode("waiting2"))
            requestedAfter.peers shouldBe Set(peerNode("peer"), peerNode("waiting1"))
          }
          it("timestamp is reset") {
            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
            val initTime = timer.clock
              .realTime(TimeUnit.MILLISECONDS)
              .unsafeRunSync
            val requested = RequestState(
              timestamp = initTime - timeout.toMillis - 1,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            currentRequests.set(Map(hash -> requested)).unsafeRunSync
            // when
            blockRetriever.requestAll(timeout).unsafeRunSync
            // then
            val Some(requestedAfter) = currentRequests.get.unsafeRunSync.get(hash)
            requestedAfter.timestamp != initTime shouldBe true
          }
        }
        describe("if waiting list has no peers left") {
          it("should broadcast requests to other peers") {
            // given
            val waitingList = List.empty[PeerNode]
            val requested = RequestState(
              timestamp = timedOut,
              peers = Set(peerNode("peer")),
              waitingList = waitingList
            )
            currentRequests.set(Map(hash -> requested)).unsafeRunSync
            // when
            blockRetriever.requestAll(timeout).unsafeRunSync
            // then
            val (_, msg) = transportLayer.getRequest(0)
            toHasBlockRequest(msg).hash should be(hash)
            transportLayer.requests.size should be(1)
          }
        }
        it(
          "should remove the entry from the requested block lists when block is in casper buffer and after timeout"
        ) {
          // given
          val waitingList = List.empty[PeerNode]
          val requested = RequestState(
            timestamp = timedOut,
            peers = Set(peerNode("peer")),
            waitingList = waitingList
          )
          currentRequests.set(Map(hash -> requested)).unsafeRunSync
          // when
          blockRetriever.requestAll(timeout).unsafeRunSync
          // then
          val requestedBlocksMapAfter = currentRequests.get.unsafeRunSync
          requestedBlocksMapAfter.size should be(0)
        }
      }
    }
  }
}
