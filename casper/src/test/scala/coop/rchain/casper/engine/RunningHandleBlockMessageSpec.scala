package coop.rchain.casper.engine

import Running.{Requested, RequestedBlocks}
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.{BlockStatus, Valid}
import coop.rchain.casper.protocol._
import coop.rchain.comm.{CommError, Endpoint, NodeIdentifier, PeerNode}, CommError._
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.rp.{ProtocolHelper, RPConf}, ProtocolHelper.toPacket
import coop.rchain.shared._
import coop.rchain.p2p.EffectsTestInstances.{LogStub, TransportLayerStub}
import coop.rchain.models.BlockHash.BlockHash
import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalatest._
import cats._, cats.data._, cats.implicits._

import scala.collection.mutable.{Map => MutableMap}

class RunningHandleBlockMessageSpec extends FunSpec with BeforeAndAfterEach with Matchers {

  val hash = ByteString.copyFrom("hash", "UTF-8")
  val bm   = BlockMessage(hash)

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  describe("Running") {
    describe("handleBlockMessage") {
      it("should remove entry from requested blocks once attempted to add to Casper") {
        // given
        val sender                   = peerNode("peer", 40400)
        val requestedBefore          = Map(hash -> Requested(timestamp = 0))
        implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
        // when
        Running.handleBlockMessage[Coeval](sender, bm)(alwaysDoesntContain, alwaysAdd).apply()
        // then
        requestedBlocks.read.apply().size should be(0)
      }
    }
  }

  val local: PeerNode = peerNode("src", 40400)
  val networkId       = "nid"
  val conf            = RPConf(local, networkId, null, null, 0, null)

  // implicit private val askConf   = new ConstApplicativeAsk[Coeval, RPConf](conf)
  implicit private val log       = new LogStub[Coeval]
  implicit private val transport = new TransportLayerStub[Coeval]

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)
  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

  private def initRequestedBlocks(
      init: Map[BlockHash, Requested]
  ): RequestedBlocks[Coeval] =
    Cell.unsafe[Coeval, Map[BlockHash, Running.Requested]](init)

  private def alwaysSuccess: PeerNode => Protocol => CommErr[Unit] = kp(kp(Right(())))
  private def alwaysDoesntContain: BlockHash => Coeval[Boolean]    = kp(false.pure[Coeval])
  private def alwaysAdd: BlockMessage => Coeval[BlockStatus]       = kp(Valid.pure[Coeval])
}
