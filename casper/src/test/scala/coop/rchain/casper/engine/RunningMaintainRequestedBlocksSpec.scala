package coop.rchain.casper.engine

import Running.{Requested, RequestedBlocks}
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}, CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}, ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.TransportLayerStub
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalatest._
import scala.concurrent.duration._
import cats._, cats.data._, cats.implicits._

import scala.collection.mutable.{Map => MutableMap}

class RunningMaintainRequestedBlocksSpec extends FunSpec with BeforeAndAfterEach with Matchers {

  val hash = ByteString.copyFrom("hash", "UTF-8")

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  describe("Running") {
    describe("maintainRequestedBlocks, for every block that was requested") {
      describe("if block request is still within a timeout") {
        it("should keep the request not touch")(pending)
        describe("if block was not delivered within given timeout") {
          describe("if waiting list is not empty") {
            it("should request block from first peer on a waiting list") {
              // given
              val requestedTs = System.currentTimeMillis - (2 * Running.timeout.toMillis)
              val waitingList = List(peerNode("waiting1"), peerNode("waiting2"))
              val requested = Requested(
                timestamp = requestedTs,
                peers = Set(peerNode("peer")),
                waitingList = waitingList
              )
              implicit val requestedBlocks = initRequestedBlocks(init = Map(hash -> requested))
              // when
              Running.maintainRequestedBlocks[Coeval].apply()
              // then
              val (recipient, msg) = transport.getRequest(0)
              toBlockRequest(msg).hash should be(hash)
              recipient shouldBe waitingList(0)
              transport.requests.size shouldBe 1
            }
            it("should move that peer from the waiting list to the requested set")(pending)
            it("timestamp is reset")(pending)
          }
          describe("if waiting list IS empty") {
            it("log a warning to the user")(pending)
            it("remove the entry from the requested block lists")(pending)
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

  private def initRequestedBlocks(
      init: Map[BlockHash, Requested]
  ): RequestedBlocks[Coeval] =
    Cell.unsafe[Coeval, Map[BlockHash, Running.Requested]](init)

  private def toBlockRequest(protocol: Protocol): BlockRequest =
    packetToBlockRequest(toPacket(protocol).right.get).get

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int = 40400): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))

}
