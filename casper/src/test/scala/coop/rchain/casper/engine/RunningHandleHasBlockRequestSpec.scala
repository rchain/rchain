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
  val hb   = HasBlock(hash)

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  describe("Running") {
    describe("handleHasBlockRequest") {
      describe("if given block is stored in BlockStore") {
        it("should send back HasBlock message to the sender")(pending)
      }
      describe("if given block is not stored in BlockStore") {
        it("should do nothing")(pending)
      }
    }
  }
  val local: PeerNode = peerNode("src", 40400)
  val networkId       = "nid"
  val conf            = RPConf(local, networkId, null, null, 0, null)

  // implicit private val askConf   = new ConstApplicativeAsk[Coeval, RPConf](conf)
  implicit private val transport = new TransportLayerStub[Coeval]

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
}
