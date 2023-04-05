package coop.rchain.casper.sync

import cats.effect.IO
import com.google.protobuf.ByteString
import coop.rchain.casper.blocks.BlockRetriever
import coop.rchain.casper.blocks.BlockRetriever.{RequestState, RequestedBlocks}
import coop.rchain.casper.protocol._
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.{Endpoint, NodeIdentifier, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances.{createRPConfAsk, LogStub, TransportLayerStub}
import coop.rchain.shared.{Log, Time}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.Ref

class BlockRetrieverSpec extends AnyFunSpec with BeforeAndAfterEach with Matchers {

  object testReason extends BlockRetriever.AdmitHashReason
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  val hash                 = ByteString.copyFrom("newHash", "utf-8")
  val local: PeerNode      = peerNode("src", 40400)
  val peer: PeerNode       = peerNode("peer", 40400)
  val secondPeer: PeerNode = peerNode("secondPeer", 40400)

  implicit val log: Log[IO] = new LogStub
  implicit val metrics      = new Metrics.MetricsNOP[IO]
  implicit val currentRequests: RequestedBlocks[IO] =
    Ref.unsafe[IO, Map[BlockHash, RequestState]](Map.empty[BlockHash, RequestState])
  implicit val connectionsCell: ConnectionsCell[IO] =
    Ref.unsafe[IO, Connections](List(local))
  implicit val transportLayer = new TransportLayerStub[IO]
  implicit val rpConf         = createRPConfAsk[IO](local)
  import coop.rchain.shared.RChainScheduler._
  implicit val time           = Time.fromTimer[IO]
  implicit val commUtil       = CommUtil.of[IO]
  implicit val blockRetriever = BlockRetriever.of[IO]

  override def beforeEach(): Unit = {
    transportLayer.reset()
    transportLayer.setResponses(_ => p => Right(()))
    currentRequests.set(Map.empty).unsafeRunSync
  }

  describe("BlockRetriever admitting hash") {
    describe("when hash is unknown") {

      it("should add record for hash") {
        blockRetriever.admitHash(hash, peer = None, admitHashReason = testReason).unsafeRunSync
        val requests = currentRequests.get.unsafeRunSync
        requests.contains(hash) should be(true)
      }

      describe("when source peer is unknown") {
        it("should broadcast HasBlockRequest and only HasBlockRequest") {
          blockRetriever.admitHash(hash, peer = None, admitHashReason = testReason).unsafeRunSync
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
          blockRetriever.admitHash(hash, Some(peer), admitHashReason = testReason).unsafeRunSync
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
            .unsafeRunSync
          val status = blockRetriever
            .admitHash(hash, peer = None, admitHashReason = testReason)
            .unsafeRunSync
          status.status equals BlockRetriever.Ignore should be(true)
        }
      }

      describe("when source peer is known") {
        it("should request block from peer if sources list was empty") {
          blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .unsafeRunSync
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
            .unsafeRunSync
          val status = blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .unsafeRunSync
          status.status equals BlockRetriever.Ignore should be(true)
        }

        it("should add peer to sources list if it is absent") {
          blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .unsafeRunSync
          blockRetriever
            .admitHash(hash, peer = Some(secondPeer), admitHashReason = testReason)
            .unsafeRunSync
          val requests = currentRequests.get.unsafeRunSync
          val peerSize = requests(hash).waitingList.size
          peerSize should be(2)
        }

        it("should NOT request for block from peer if sources list was not empty") {
          blockRetriever
            .admitHash(hash, peer = Some(peer), admitHashReason = testReason)
            .unsafeRunSync
          blockRetriever
            .admitHash(hash, peer = Some(secondPeer), admitHashReason = testReason)
            .unsafeRunSync
          transportLayer.requests.size should be(1)
        }
      }
    }
  }
}
