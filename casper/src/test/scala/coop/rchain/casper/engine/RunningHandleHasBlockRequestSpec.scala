package coop.rchain.casper.engine

import coop.rchain.catscontrib.ski._
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}
import CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}
import ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.TransportLayerStub
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import coop.rchain.p2p.EffectsTestInstances
import cats.Eval
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import coop.rchain.catscontrib.effect.implicits.sEval

import scala.collection.immutable.ArraySeq

class RunningHandleHasBlockRequestSpec extends AnyFunSpec with BeforeAndAfterEach with Matchers {

  val hash = ByteString.copyFrom("hash", "UTF-8")
  val hbr  = HasBlockRequest(hash)

  val local: PeerNode = peerNode("src", 40400)
  val networkId       = "nid"
  val conf            = RPConf(local, networkId, null, null, 0, null)

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(ArraySeq.unsafeWrapArray(name.getBytes)), endpoint(port))

  def toHasBlock(protocol: Protocol): HasBlock =
    HasBlock.from(convert[PacketTypeTag.HasBlock.type](toPacket(protocol).toOption.get).get)

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))

  implicit private val askConf   = new ConstApplicativeAsk[Eval, RPConf](conf)
  implicit private val transport = new TransportLayerStub[Eval]

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  describe("Running") {
    describe("handleHasBlockRequest") {
      describe("if given block is stored") {
        it("should send back HasBlock message to the sender") {
          // given
          val sender                                  = peerNode("peer", 40400)
          val blockLookup: BlockHash => Eval[Boolean] = kp(Eval.now(true))
          // then
          NodeRunning.handleHasBlockRequest[Eval](sender, hbr)(blockLookup).value
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
          val sender                                  = peerNode("peer", 40400)
          val blockLookup: BlockHash => Eval[Boolean] = kp(Eval.now(false))
          // then
          NodeRunning.handleHasBlockRequest[Eval](sender, hbr)(blockLookup).value
          // then
          transport.requests.size should be(0)
        }
      }
    }
  }
}
