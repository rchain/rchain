//package coop.rchain.casper.engine
//
//import coop.rchain.catscontrib.ski._
//import coop.rchain.casper.protocol._
//import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}
//import CommError._
//import coop.rchain.comm.protocol.routing.Protocol
//import coop.rchain.comm.rp.{ProtocolHelper, RPConf}
//import ProtocolHelper.toPacket
//import cats.effect.concurrent.Ref
//import coop.rchain.shared._
//import coop.rchain.p2p.EffectsTestInstances.{
//  createRPConfAsk,
//  LogStub,
//  LogicalTime,
//  TransportLayerStub
//}
//import coop.rchain.models.BlockHash.BlockHash
//import com.google.protobuf.ByteString
//import coop.rchain.casper.engine
//import coop.rchain.casper.engine.BlockRetriever.RequestState
//import coop.rchain.casper.engine.Running.{
//  BlockIsInCasperBuffer,
//  DoNotIgnore,
//  IgnoreCasperMessageStatus
//}
//import coop.rchain.casper.util.comm.CommUtil
//import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
//import coop.rchain.metrics.Metrics
//import monix.eval.Task
//import org.scalatest._
//import monix.execution.Scheduler.Implicits.global
//import cats.implicits._
//
//class RunningHandleHasBlockSpec extends FunSpec with BeforeAndAfterEach with Matchers {
//
//  val local: PeerNode = peerNode("src", 40400)
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
//  val hash = ByteString.copyFrom("hash", "UTF-8")
//  val hb   = HasBlock(hash)
//
//  val networkId = "nid"
//  val conf      = RPConf(local, networkId, null, null, 0, null)
//
//  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
//  private def peerNode(name: String, port: Int): PeerNode =
//    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
//
//  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
//  private def alwaysDoNotIgnoreF: BlockHash => Task[Boolean]       = _ => false.pure[Task]
//  override def beforeEach(): Unit = {
//    transportLayer.reset()
//    transportLayer.setResponses(alwaysSuccess)
//    time.reset()
//  }
//
//  describe("BlockRetriever") {
//    it("should store on a waiting list and don't request if RequestState by different peer") {
//      // given
//      val sender    = peerNode("somePeer", 40400)
//      val otherPeer = peerNode("otherPeer", 40400)
//      val requestStateBefore =
//        Map(
//          hash -> RequestState(timestamp = System.currentTimeMillis, waitingList = List(otherPeer))
//        )
//      currentRequests.set(requestStateBefore).runSyncUnsafe()
//      // when
//      Running.handleHasBlockMessage[Task](sender, hb)(alwaysDoNotIgnoreF).runSyncUnsafe()
//      // then
//      transportLayer.requests shouldBe empty
//
//      val requestStateAfter = currentRequests.get.runSyncUnsafe().get(hash).get
//      requestStateAfter.waitingList.size should be(2)
//    }
//
//    it("should request block and add peer to waitingList list if peers list is empty") {
//      // given
//      val sender = peerNode("somePeer", 40400)
//      val requestStateBefore =
//        Map(
//          hash -> RequestState(
//            timestamp = System.currentTimeMillis,
//            peers = Set.empty,
//            waitingList = List.empty
//          )
//        )
//      currentRequests.set(requestStateBefore).runSyncUnsafe()
//      // when
//      Running.handleHasBlockMessage[Task](sender, hb)(alwaysDoNotIgnoreF).runSyncUnsafe()
//      // then
//      val (recipient, msg) = transportLayer.getRequest(0)
//      // assert RequestState
//      val br = BlockRequest.from(
//        convert[PacketTypeTag.BlockRequest.type](toPacket(msg).right.get).get
//      )
//      br.hash shouldBe hash
//      recipient shouldBe sender
//      transportLayer.requests.size shouldBe 1
//      // assert RequestState information stored
//      val requestStateAfter = currentRequests.get.runSyncUnsafe().get(hash).get
//      requestStateAfter.waitingList should be(List(sender))
//    }
//  }
//
//  describe("if there is no yet an an entry in the RequestState blocks") {
//    it("should request block and store information about RequestState block") {
//      // given
//      val sender             = peerNode("somePeer", 40400)
//      val requestStateBefore = Map.empty[BlockHash, RequestState]
//      currentRequests.set(requestStateBefore).runSyncUnsafe()
//      // when
//      Running.handleHasBlockMessage[Task](sender, hb)(alwaysDoNotIgnoreF).runSyncUnsafe()
//      // then
//      val (recipient, msg) = transportLayer.getRequest(0)
//      // assert RequestState
//      val br = BlockRequest.from(
//        convert[PacketTypeTag.BlockRequest.type](toPacket(msg).right.get).get
//      )
//      br.hash shouldBe hash
//      recipient shouldBe sender
//      transportLayer.requests.size shouldBe 1
//      // assert RequestState informaton stored
//      val requestStateAfter = currentRequests.get.runSyncUnsafe().get(hash).get
//      requestStateAfter.waitingList should be(List(sender))
//    }
//
//    describe("if casper does NOT contain block with given hash") {
//      describe("if there is already an entry in the RequestState blocks") {
//        it("should ignore if peer on the RequestState peers list") {
//          // given
//          val sender                                     = peerNode("somePeer", 40400)
//          val casperContains: BlockHash => Task[Boolean] = _ => true.pure[Task]
//          // when
//          Running.handleHasBlockMessage[Task](sender, hb)(casperContains).runSyncUnsafe()
//          // then
//          transportLayer.requests shouldBe empty
//        }
//      }
//    }
//  }
//
//  describe("Running") {
//    describe("handleHasBlock") {
//      it("should not call send hash to BlockReceiver if it is ignorable hash") {
//        // given
//        val casperContains: BlockHash => Task[Boolean] = _ => true.pure[Task]
//        // when
//        Running.handleHasBlockMessage[Task](null, hb)(casperContains).runSyncUnsafe()
//        // then
//        transportLayer.requests shouldBe empty
//      }
//    }
//  }
//}
