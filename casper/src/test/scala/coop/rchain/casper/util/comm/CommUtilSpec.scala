package coop.rchain.casper.engine

import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{CommUtil, _}
import coop.rchain.catscontrib.ski._
import coop.rchain.comm.CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.ProtocolHelper.toPacket
import coop.rchain.comm.rp.RPConf
import coop.rchain.comm.{Endpoint, NodeIdentifier, PeerNode}
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime, TransportLayerStub}
import coop.rchain.shared._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CommUtilSpec extends AnyFunSpec with BeforeAndAfterEach with Matchers {

  // TODO this is testing how CommUtil manipulates RequestedBlocks,
  //  is not a valid test anymore with introduction of BlockRetriever and us moving towards more use of TransportLayer.
  //  move to BlockRetriever tests.
  /*
  val hash = ByteString.copyFrom("hash", "UTF-8")

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
    log.reset()
  }

  describe("CommUtil") {
    describe("sendBlockRequest") {
      describe("if given block was not yet requested") {
        it("broadcast HasBlockRequest to random peers") {
          val peers = List(
            peerNode("peer1", 40400),
            peerNode("peer2", 40400)
          )
          implicit val requestedBlocks = initRequestedBlocks()
          implicit val connectionsCell = initConnectionsCell(connections = peers)
          implicit val commUtil        = CommUtil.of[IO]
          // when
          CommUtil[IO].sendBlockRequest(hash).unsafeRunSync
          // then
          val requested = transport.requests
            .map(_.msg)
            .map(toHasBlockRequest)
            .map(_.hash)
            .toSet
          requested should be(Set(hash))
          val requestedPeers = transport.requests.map(_.peer)
          requestedPeers should contain(peers(0))
          requestedPeers should contain(peers(1))
          transport.requests.size shouldBe 2
        }
        it("should log to INFO that request was made") {
          val peers = List(
            peerNode("peer1", 40400),
            peerNode("peer2", 40400)
          )
          implicit val requestedBlocks = initRequestedBlocks()
          implicit val connectionsCell = initConnectionsCell(connections = peers)
          implicit val commUtil        = CommUtil.of[IO]
          // when
          CommUtil[IO].sendBlockRequest(hash).unsafeRunSync
          // then
          log.infos contains (s"Requested missing block ${PrettyPrinter.buildString(hash)} from peers")
        }
        it("should create new entry in 'requested blocks'") {
          // given
          implicit val requestedBlocks = initRequestedBlocks()
          implicit val connectionsCell = initConnectionsCell()
          implicit val commUtil        = CommUtil.of[IO]
          // when
          CommUtil[IO].sendBlockRequest(hash).unsafeRunSync
          // then
          requestedBlocks.read.unsafeRunSync.contains(hash) should be(true)
        }
      }
      describe("if given block was already requested") {
        it("should do nothing") {
          val peers = List(
            peerNode("peer1", 40400),
            peerNode("peer2", 40400)
          )
          val requestedBefore = Map(
            hash -> Requested(
              timestamp = System.currentTimeMillis,
              peers = Set(peerNode("peer2", 40400))
            )
          )
          implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
          implicit val connectionsCell = initConnectionsCell(connections = peers)
          implicit val commUtil        = CommUtil.of[IO]
          // when
          CommUtil[IO].sendBlockRequest(hash).unsafeRunSync
          // then
          transport.requests.size shouldBe 0
          log.infos.size shouldBe 0
        }
      }
    }
  }

  val local: PeerNode  = peerNode("src", 40400)
  val networkId        = "nid"
  val maxNoConnections = 10
  val conf             = RPConf(local, networkId, null, null, maxNoConnections, null)

  implicit val transport = new TransportLayerStub[IO]
  implicit val askConf   = new ConstApplicativeAsk[IO, RPConf](conf)
  implicit val log       = new LogStub[IO]
  implicit val time      = new LogicalTime[IO]
  implicit val metrics   = new MetricsNOP[IO]

  private def initRequestedBlocks(
      init: Map[BlockHash, Requested] = Map.empty
  ): RequestedBlocks[IO] =
    Cell.unsafe[IO, Map[BlockHash, Running.Requested]](init)
  private def initConnectionsCell(connections: Connections = List.empty) =
    Cell.unsafe[IO, Connections](connections)
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def toHasBlockRequest(protocol: Protocol): HasBlockRequest =
    HasBlockRequest.from(
      convert[PacketTypeTag.HasBlockRequest.type](toPacket(protocol).right.get).get
    )

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
 */
}
