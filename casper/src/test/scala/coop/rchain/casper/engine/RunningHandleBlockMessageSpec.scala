package coop.rchain.casper.engine

import cats.implicits._
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.BlockStatus
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import CommError._
import cats.effect.concurrent.{Ref, Semaphore}
import coop.rchain.casper.{BlockError, ValidBlock, _}
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.RPConf
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances.{createRPConfAsk, LogStub, TransportLayerStub}
import coop.rchain.shared._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockStore, InMemBlockStore}
import coop.rchain.casper.engine.BlockRetriever.RequestState
import coop.rchain.casper.engine.Running.{
  BlockIsWaitingForCasper,
  DoNotIgnore,
  IgnoreCasperMessageStatus
}
import coop.rchain.casper.helper.NoOpsCasperEffect
import coop.rchain.casper.util.TestTime
import coop.rchain.casper.util.comm.CommUtil
import monix.eval.Task
import org.scalatest._
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.metrics.Metrics
import monix.execution.Scheduler.Implicits.global

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class RunningHandleBlockMessageSpec extends FunSpec with BeforeAndAfterEach with Matchers {
//
//  implicit val log              = new LogStub[Task]
//  implicit val transportLayer   = new TransportLayerStub[Task]
//  implicit val metrics          = new Metrics.MetricsNOP[Task]
//  implicit val blockMap         = Ref.unsafe[Task, Map[BlockHash, BlockMessageProto]](Map.empty)
//  implicit val approvedBlockRef = Ref.unsafe[Task, Option[ApprovedBlock]](None)
//  implicit val blockStore       = InMemBlockStore.create[Task]
//
//  val local: PeerNode = peerNode("src", 40400)
//  implicit val currentRequests: engine.BlockRetriever.RequestedBlocks[Task] =
//    Ref.unsafe[Task, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])
//  implicit val connectionsCell: ConnectionsCell[Task] =
//    Cell.unsafe[Task, Connections](List(local))
//  implicit val rpConf         = createRPConfAsk[Task](local)
//  implicit val time           = TestTime.instance
//  implicit val commUtil       = CommUtil.of[Task]
//  implicit val blockRetriever = BlockRetriever.of[Task]
//
//  val sender     = peerNode("peer", 40400)
//  val validBlock = getRandomBlock()
//
//  val sentToCasper: mutable.HashSet[BlockHash] = mutable.HashSet.empty[BlockHash]
//
//  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
//  private def peerNode(name: String, port: Int): PeerNode =
//    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))
//
//  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
//
//  private def alwaysIgnoreF: BlockHash => Task[Running.IgnoreCasperMessageStatus] =
//    kp(IgnoreCasperMessageStatus(true, BlockIsWaitingForCasper).pure[Task])
//  private def alwaysDoNotIgnoreF: BlockHash => Task[Running.IgnoreCasperMessageStatus] =
//    kp(IgnoreCasperMessageStatus(false, DoNotIgnore).pure[Task])
//
//  private def alwaysValidBlockF: (BlockMessage, PeerNode) => Task[Boolean] =
//    kp2[BlockMessage, PeerNode, Task[Boolean]](true.pure[Task])
//  private def alwaysBadBlockF: (BlockMessage, PeerNode) => Task[Boolean] =
//    kp2[BlockMessage, PeerNode, Task[Boolean]](false.pure[Task])
//
//  private def casperAddF(bh: BlockHash): Task[Unit] = (sentToCasper += bh).pure[Task].void
//  private val hashHandlerLock                       = TrieMap.empty[BlockHash, Semaphore[Task]]
//
//  override def beforeEach(): Unit = {
//    transportLayer.reset
//    transportLayer.setResponses(alwaysSuccess)
//    sentToCasper.remove(validBlock.blockHash)
//    blockStore.clear.runSyncUnsafe()
//  }

//  describe("Running") {
//    describe("handleBlockMessage") {
//      it("should pass good block to Casper and mark block as received in BlockRetriever") {
//        Running
//          .handleBlockMessage[Task](validBlock, sender)(
//            alwaysDoNotIgnoreF,
//            alwaysValidBlockF,
//            casperAddF,
//            hashHandlerLock
//          )
//          .runSyncUnsafe()
//        sentToCasper.contains(validBlock.blockHash) should be(true)
//        blockRetriever.isReceived(validBlock.blockHash).unsafeRunSync should be(true)
//      }
//
//      it("should drop not valid messages") {
//        val b = getRandomBlock().copy(shardId = "wrongshard")
//        Running
//          .handleBlockMessage[Task](b, sender)(
//            alwaysDoNotIgnoreF,
//            alwaysBadBlockF,
//            casperAddF,
//            hashHandlerLock
//          )
//          .unsafeRunSync
//        sentToCasper.contains(b.blockHash) should be(false)
//      }
//
//      it("should not pass message to Casper when should be ignored") {
//        val b = getRandomBlock().copy(shardId = "wrongshard")
//        Running
//          .handleBlockMessage[Task](b, sender)(
//            alwaysIgnoreF,
//            alwaysValidBlockF,
//            casperAddF,
//            hashHandlerLock
//          )
//          .unsafeRunSync
//        sentToCasper.contains(b.blockHash) should be(false)
//      }
//
//      // TODO this is after LFS comlete
//      ignore("should drop old ignorable blocks") {}
//    }
//  }
}
