package coop.rchain.casper.sync

import cats.effect.concurrent.Ref
import com.google.protobuf.ByteString
import coop.rchain.casper.engine
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.engine.BlockRetriever.RequestState
import coop.rchain.casper.protocol._
import coop.rchain.comm.{Endpoint, NodeIdentifier, PeerNode}
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances.{createRPConfAsk, LogStub, TransportLayerStub}
import coop.rchain.shared.{Cell, Time}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BlockRetrieverSpec extends AnyFunSpec with BeforeAndAfterEach with Matchers {

  object testReason extends BlockRetriever.AdmitHashReason
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  val hash                 = ByteString.copyFrom("newHash", "utf-8")
  val local: PeerNode      = peerNode("src", 40400)
  val peer: PeerNode       = peerNode("peer", 40400)
  val secondPeer: PeerNode = peerNode("secondPeer", 40400)

  implicit val log     = new LogStub[Task]
  implicit val metrics = new Metrics.MetricsNOP[Task]
  implicit val currentRequests: engine.BlockRetriever.RequestedBlocks[Task] =
    Ref.unsafe[Task, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])
  implicit val connectionsCell: ConnectionsCell[Task] =
    Cell.unsafe[Task, Connections](List(local))
  implicit val transportLayer = new TransportLayerStub[Task]
  implicit val rpConf         = createRPConfAsk[Task](local)
  implicit val time           = Time.fromTimer[Task]
  implicit val commUtil       = CommUtil.of[Task]
  implicit val blockRetriever = BlockRetriever.of[Task]

  override def beforeEach(): Unit = {
    transportLayer.reset()
    transportLayer.setResponses(_ => p => Right(()))
    currentRequests.set(Map.empty).runSyncUnsafe()
  }

  describe("BlockRetriever admitting hash") {
    describe("when hash is unknown") {

      it("should add record for hash") {
        blockRetriever.admitHash(hash, peer = None, admitHashReason = testReason).runSyncUnsafe()
        val requests = currentRequests.get.runSyncUnsafe()
        requests.contains(hash) should be(true)
      }

      describe("when source peer is unknown") {
        it("should broadcast HasBlockRequest and only HasBlockRequest") {
          blockRetriever.admitHash(hash, peer = None, admitHashReason = testReason).runSyncUnsafe()
          val (_, msg) = transportLayer.getRequest(0)
          val hbr = HasBlockRequest.from(
            convert[PacketTypeTag.HasBlockRequest.type](toPacket(msg).right.get).get
          )
          (hbr.hash equals hash) should be(true)
          transportLayer.requests.size should be(1)
        }
      }

      describe("when source peer is known") {
        it("should send BlockRequest and only BlockRequest") {
          blockRetriever.admitHash(hash, Some(peer), admitHashReason = testReason).runSyncUnsafe()
          val (recipient, msg) = transportLayer.getRequest(0)
          val br = BlockRequest.from(
            convert[PacketTypeTag.BlockRequest.type](toPacket(msg).right.get).get
          )
          (br.hash equals hash) should be(true)
          (recipient equals peer) should be(true)
          transportLayer.requests.size should be(1)
        }
      }
    }

    describe("when hash is known") {
      blockRetriever.admitHash(hash, peer = None, admitHashReason = testReason)

      describe("when source peer is unknown") {
        it("should ignore hash") {
          blockRetriever
            .admitHash(hash, peer = None, admitHashReason = testReason)
            .runSyncUnsafe()
          val status = blockRetriever
            .admitHash(hash, peer = None, admitHashReason = testReason)
            .runSyncUnsafe()
          status.status equals BlockRetriever.Ignore should be(true)
        }
      }

      describe("when source peer is known") {
        it("should request block from peer if sources list was empty") {
          blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .runSyncUnsafe()
          val (recipient, msg) = transportLayer.getRequest(0)
          val br = BlockRequest.from(
            convert[PacketTypeTag.BlockRequest.type](toPacket(msg).right.get).get
          )
          (br.hash equals hash) should be(true)
          (recipient equals peer) should be(true)
          transportLayer.requests.size should be(1)
        }

        it("should ignore hash if peer is already in sources list") {
          blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .runSyncUnsafe()
          val status = blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .runSyncUnsafe()
          status.status equals BlockRetriever.Ignore should be(true)
        }

        it("should add peer to sources list if it is absent") {
          blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .runSyncUnsafe()
          blockRetriever
            .admitHash(hash, peer = Some(secondPeer), admitHashReason = testReason)
            .runSyncUnsafe()
          val requests = currentRequests.get.runSyncUnsafe()
          val peerSize = requests(hash).waitingList.size
          peerSize should be(2)
        }

        it("should NOT request for block from peer if sources list was not empty") {
          blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .runSyncUnsafe()
          blockRetriever
            .admitHash(hash, peer = Some(secondPeer), admitHashReason = testReason)
            .runSyncUnsafe()
          transportLayer.requests.size should be(1)
        }
      }
    }
  }
}
