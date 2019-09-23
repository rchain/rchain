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
import cats._, cats.data._, cats.implicits._

import scala.collection.mutable.{Map => MutableMap}

class RunningHandleHasBlockRequestSpec extends FunSpec with BeforeAndAfterEach with Matchers {

  val hash = ByteString.copyFrom("hash", "UTF-8")
  val hbr  = HasBlockRequest(hash)

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  describe("Running") {
    describe("handleHasBlockRequest") {
      describe("if given block is stored") {
        it("should send back HasBlock message to the sender") {
          // given
          val sender                                    = peerNode("peer", 40400)
          val blockLookup: BlockHash => Coeval[Boolean] = kp(Coeval(true))
          // then
          Running.handleHasBlockRequest[Coeval](sender, hbr)(blockLookup).apply()
          // then
          val (peer, msg) = transport.getRequest(0)
          peer should be(sender)
          toHasBlock(msg).hash should be(hash)
          transport.requests.size should be(1)
        }
      }
      describe("if given block is not stored in BlockStore") {
        it("should do nothing") {
          // given
          val sender                                    = peerNode("peer", 40400)
          val blockLookup: BlockHash => Coeval[Boolean] = kp(Coeval(false))
          // then
          Running.handleHasBlockRequest[Coeval](sender, hbr)(blockLookup).apply()
          // then
          transport.requests.size should be(0)
        }
      }
    }
  }
  val local: PeerNode = peerNode("src", 40400)
  val networkId       = "nid"
  val conf            = RPConf(local, networkId, null, null, 0, null)

  implicit private val askConf   = new ConstApplicativeAsk[Coeval, RPConf](conf)
  implicit private val transport = new TransportLayerStub[Coeval]

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  def toHasBlock(protocol: Protocol): HasBlock =
    HasBlock.from(packetToHasBlock(toPacket(protocol).right.get).get)

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
}
