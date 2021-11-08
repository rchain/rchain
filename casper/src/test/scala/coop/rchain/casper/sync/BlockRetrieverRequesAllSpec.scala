//package coop.rchain.casper.sync
//
//import cats.effect.concurrent.Ref
//import com.google.protobuf.ByteString
//import coop.rchain.casper.{engine, PrettyPrinter}
//import coop.rchain.casper.engine.BlockRetriever.{RequestState, RequestedBlocks}
//import coop.rchain.casper.engine.Setup.peerNode
//import coop.rchain.casper.engine.{BlockRetriever, Running}
//import coop.rchain.casper.protocol._
//import coop.rchain.casper.util.TestTime
//import coop.rchain.casper.util.comm.CommUtil
//import coop.rchain.catscontrib.ski._
//import coop.rchain.comm.CommError._
//import coop.rchain.comm.protocol.routing.Protocol
//import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
//import coop.rchain.comm.rp.ProtocolHelper.toPacket
//import coop.rchain.comm.rp.{ProtocolHelper, RPConf}
//import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}
//import coop.rchain.metrics.Metrics
//import coop.rchain.models.BlockHash.BlockHash
//import coop.rchain.p2p.EffectsTestInstances.{
//  createRPConfAsk,
//  LogStub,
//  LogicalTime,
//  TransportLayerStub
//}
//import coop.rchain.shared._
//import monix.eval.Task
//import org.scalatest._
//import monix.execution.Scheduler.Implicits.global
//
//import scala.concurrent.duration._
//
//class BlockRetrieverRequesAllSpec extends FunSpec with BeforeAndAfterEach with Matchers {
//
//  object testReason extends BlockRetriever.AdmitHashReason
//
//  val hash                    = ByteString.copyFrom("newHash", "utf-8")
//  val timeout: FiniteDuration = 240.seconds
//  val local: PeerNode         = peerNode("src", 40400)
//
//  implicit val log     = new LogStub[Task]
//  implicit val metrics = new Metrics.MetricsNOP[Task]
//  implicit val currentRequests: engine.BlockRetriever.RequestedBlocks[Task] =
//    Ref.unsafe[Task, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])
//  implicit val connectionsCell: ConnectionsCell[Task] =
//    Cell.unsafe[Task, Connections](List(local))
//  implicit val transportLayer = new TransportLayerStub[Task]
//  implicit val rpConf         = createRPConfAsk[Task](local)
//  implicit val time           = new LogicalTime[Task]
//  implicit val commUtil       = CommUtil.of[Task]
//  implicit val blockRetriever = BlockRetriever.of[Task]
//
//  val networkId = "nid"
//  val conf      = RPConf(local, networkId, null, null, 0, null)
//
//  private def toBlockRequest(protocol: Protocol): BlockRequest =
//    BlockRequest.from(convert[PacketTypeTag.BlockRequest.type](toPacket(protocol).right.get).get)
//  private def toHasBlockRequest(protocol: Protocol): HasBlockRequest =
//    HasBlockRequest.from(
//      convert[PacketTypeTag.HasBlockRequest.type](toPacket(protocol).right.get).get
//    )
//
//  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
//  private def peerNode(name: String, port: Int = 40400): PeerNode =
//    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
//
//  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
//
//  private def timedOut: Long    = time.clock - (2 * timeout.toMillis)
//  private def notTimedOut: Long = time.clock - 1
//
//  override def beforeEach(): Unit = {
//    transportLayer.reset()
//    transportLayer.setResponses(alwaysSuccess)
//    log.reset()
//    time.reset()
//  }
//
//  describe("Running") {
//    describe("maintainRequestedBlocks, for every block that was requested") {
//      describe("if block request is still within a timeout") {
//        it("should keep the request not touch") {
//          val requested = RequestState(timestamp = notTimedOut)
//          currentRequests.set(Map(hash -> requested)).runSyncUnsafe()
//          // when
//          blockRetriever.requestAll(timeout).runSyncUnsafe()
//          // then
//          val requestedBlocksMapAfter = currentRequests.get.runSyncUnsafe()
//          requestedBlocksMapAfter.size should be(1)
//        }
//      }
//      describe("if block was not delivered within given timeout") {
//        describe("if waiting list is not empty") {
//          it("should request block from first peer on a waiting list") {
//            // given
//            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
//            val requested = RequestState(
//              timestamp = timedOut,
//              peers = Set(peerNode("peer")),
//              waitingList = waitingList
//            )
//            currentRequests.set(Map(hash -> requested)).runSyncUnsafe()
//            // when
//            blockRetriever.requestAll(timeout).runSyncUnsafe()
//            // then
//            val (recipient, msg) = transportLayer.getRequest(0)
//            toBlockRequest(msg).hash should be(hash)
//            recipient shouldBe waitingList.head
//            transportLayer.requests.size shouldBe 1
//          }
//          it("should move that peer from the waiting list to the requested set") {
//            // given
//            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
//            val requested = RequestState(
//              timestamp = timedOut,
//              peers = Set(peerNode("peer")),
//              waitingList = waitingList
//            )
//            currentRequests.set(Map(hash -> requested)).runSyncUnsafe()
//            // when
//            blockRetriever.requestAll(timeout).runSyncUnsafe()
//            // then
//            val Some(requestedAfter) = currentRequests.get.runSyncUnsafe().get(hash)
//            requestedAfter.waitingList shouldBe List(peerNode("waiting2"))
//            requestedAfter.peers shouldBe Set(peerNode("peer"), peerNode("waiting1"))
//          }
//          it("timestamp is reset") {
//            val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
//            val requested = RequestState(
//              timestamp = timedOut,
//              peers = Set(peerNode("peer")),
//              waitingList = waitingList
//            )
//            currentRequests.set(Map(hash -> requested)).runSyncUnsafe()
//            // when
//            blockRetriever.requestAll(timeout).runSyncUnsafe()
//            // then
//            val Some(requestedAfter) = currentRequests.get.runSyncUnsafe().get(hash)
//            requestedAfter.timestamp shouldBe time.clock
//          }
//        }
//        describe("if waiting list has one peer left") {
//          it("should broadcast HasBlockRequest") {
//            // given
//            val waitingList = List(peerNode("lastPeer"))
//            val requested = RequestState(
//              timestamp = timedOut,
//              peers = Set(peerNode("peer")),
//              waitingList = waitingList
//            )
//            currentRequests.set(Map(hash -> requested)).runSyncUnsafe()
//            // when
//            blockRetriever.requestAll(timeout).runSyncUnsafe()
//            // then
//            val (recipient, msg) = transportLayer.getRequest(0)
//            toBlockRequest(msg).hash should be(hash)
//            recipient shouldBe waitingList.head
//            val (_, msg1) = transportLayer.getRequest(1)
//            toHasBlockRequest(msg1).hash should be(hash)
//            transportLayer.requests.size shouldBe 2
//          }
//          it("should NOT send requests to other peers") {
//            // given
//            val waitingList = List.empty[PeerNode]
//            val requested = RequestState(
//              timestamp = timedOut,
//              peers = Set(peerNode("peer")),
//              waitingList = waitingList
//            )
//            currentRequests.set(Map(hash -> requested)).runSyncUnsafe()
//            // when
//            blockRetriever.requestAll(timeout).runSyncUnsafe()
//            // then
//            transportLayer.requests.size should be(0)
//          }
//          it(
//            "should remove the entry from the requested block lists when block is in casper buffer and after timeout"
//          ) {
//            // given
//            val waitingList = List.empty[PeerNode]
//            val requested = RequestState(
//              timestamp = timedOut,
//              inCasperBuffer = true,
//              peers = Set(peerNode("peer")),
//              waitingList = waitingList
//            )
//            currentRequests.set(Map(hash -> requested)).runSyncUnsafe()
//            // when
//            blockRetriever.requestAll(timeout).runSyncUnsafe()
//            // then
//            val requestedBlocksMapAfter = currentRequests.get.runSyncUnsafe()
//            requestedBlocksMapAfter.size should be(0)
//          }
//        }
//      }
//    }
//  }
//}
