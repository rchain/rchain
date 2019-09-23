package coop.rchain.casper.engine

import Running.{Requested, RequestedBlocks}
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}, CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}, ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime, TransportLayerStub}
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalatest._
import cats._, cats.data._, cats.implicits._

import scala.collection.mutable.{Map => MutableMap}

class CommUtilSpec extends FunSpec with BeforeAndAfterEach with Matchers {

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
          // when
          CommUtil.sendBlockRequest[Coeval](hash).apply()
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
          // when
          CommUtil.sendBlockRequest[Coeval](hash).apply()
          // then
          log.infos contains (s"Requested missing block ${PrettyPrinter.buildString(hash)} from peers")
        }
        it("should create new entry in 'requested blocks'") {
          // given
          implicit val requestedBlocks = initRequestedBlocks()
          implicit val connectionsCell = initConnectionsCell()
          // when
          CommUtil.sendBlockRequest[Coeval](hash).apply()
          // then
          requestedBlocks.read.apply().contains(hash) should be(true)
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
          // when
          CommUtil.sendBlockRequest[Coeval](hash).apply()
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

  implicit val transport = new TransportLayerStub[Coeval]
  implicit val askConf   = new ConstApplicativeAsk[Coeval, RPConf](conf)
  implicit val log       = new LogStub[Coeval]
  implicit val time      = new LogicalTime[Coeval]

  private def initRequestedBlocks(
      init: Map[BlockHash, Requested] = Map.empty
  ): RequestedBlocks[Coeval] =
    Cell.unsafe[Coeval, Map[BlockHash, Running.Requested]](init)
  private def initConnectionsCell(connections: Connections = List.empty) =
    Cell.unsafe[Coeval, Connections](connections)
  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def toHasBlockRequest(protocol: Protocol): HasBlockRequest =
    HasBlockRequest.from(packetToHasBlockRequest(toPacket(protocol).right.get).get)

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
}
