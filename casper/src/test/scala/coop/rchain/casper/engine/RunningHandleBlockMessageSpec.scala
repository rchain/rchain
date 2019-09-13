package coop.rchain.casper.engine

import cats.implicits._

import Running.{Requested, RequestedBlocks}
import coop.rchain.casper.CasperMessageFactory._
import coop.rchain.catscontrib.ski._
import coop.rchain.casper.{BlockStatus}
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.ski._
import coop.rchain.comm._
import CommError._
import coop.rchain.casper.{BlockError, ValidBlock, _}
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.RPConf
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances.{LogStub, TransportLayerStub}
import coop.rchain.shared._

import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalatest._

class RunningHandleBlockMessageSpec extends FunSpec with BeforeAndAfterEach with Matchers {

  val hash = ByteString.copyFrom("hash", "UTF-8")
  val bm   = createBlockMessage(blockHash = hash)

  override def beforeEach(): Unit = {
    transport.reset()
    transport.setResponses(alwaysSuccess)
  }

  describe("Running") {
    describe("handleBlockMessage") {
      it("should remove entry from requested blocks once block ended up in casper DAG") {
        // given
        val sender                   = peerNode("peer", 40400)
        val requestedBefore          = Map(hash -> Requested(timestamp = 0))
        implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
        // when
        Running.handleBlockMessage[Coeval](sender, bm)(alwaysDoesntContain, blockInDag).apply()
        // then
        requestedBlocks.read.apply().size should be(0)
      }

      it("should NOT remove entry from requested blocks if block did NOT ended up in casper DAG") {
        // given
        val sender                   = peerNode("peer", 40400)
        val requestedBefore          = Map(hash -> Requested(timestamp = 0))
        implicit val requestedBlocks = initRequestedBlocks(init = requestedBefore)
        // when
        Running.handleBlockMessage[Coeval](sender, bm)(alwaysDoesntContain, blockNotInDag).apply()
        // then
        requestedBlocks.read.apply().size should be(1)
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
  private def blockInDag: BlockMessage => Coeval[ValidBlockProcessing] =
    kp(ValidBlock.Valid.asRight.pure[Coeval])
  private def blockNotInDag: BlockMessage => Coeval[ValidBlockProcessing] =
    kp(BlockError.Processing.asLeft.pure[Coeval])
}
