package coop.rchain.casper.util.comm

import cats.Id
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.HashSetCasperTest
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.comm.transport
import coop.rchain.p2p.EffectsTestInstances._

import org.scalatest.{FlatSpec, Matchers}

class BlockApproverProtocolTest extends FlatSpec with Matchers {
  import BlockApproverProtocolTest._

  "BlockApproverProtocol" should "respond to valid ApprovedBlockCandidates" in {
    val n                = 8
    val (approver, node) = createIdProtocol(n)
    val unapproved       = createUnapproved(n, node.genesis)

    approver.unapprovedBlockPacketHandler(node.local)(unapprovedToPacket(unapproved))

    node.logEff.infos.exists(_.contains("Approval sent in response")) should be(true)
    node.logEff.warns.isEmpty should be(true)

    node.transportLayerEff.msgQueues(node.local).size should be(1)
  }

  it should "log a warning for invalid ApprovedBlockCandidates" in {
    val n                    = 8
    val (approver, node)     = createIdProtocol(n)
    val differentUnapproved1 = createUnapproved(n / 2, node.genesis) //wrong number of signatures
    val differentUnapproved2 = createUnapproved(n, BlockMessage.defaultInstance) //wrong block

    approver.unapprovedBlockPacketHandler(node.local)(unapprovedToPacket(differentUnapproved1))
    approver.unapprovedBlockPacketHandler(node.local)(unapprovedToPacket(differentUnapproved2))

    node.logEff.warns.count(_.contains("Received unexpected candidate")) should be(2)

    node.transportLayerEff.msgQueues(node.local).isEmpty should be(true)
  }
}

object BlockApproverProtocolTest {
  def createUnapproved(requiredSigs: Int, block: BlockMessage): UnapprovedBlock =
    UnapprovedBlock(Some(ApprovedBlockCandidate(Some(block), requiredSigs)), 0L, 0L)

  def unapprovedToPacket(u: UnapprovedBlock): Packet =
    Packet(transport.UnapprovedBlock.id, u.toByteString)

  def createIdProtocol(requiredSigs: Int): (BlockApproverProtocol[Id], HashSetCasperTestNode) = {
    import monix.execution.Scheduler.Implicits.global

    val (sk, pk) = Ed25519.newKeyPair
    val genesis  = HashSetCasperTest.createGenesis(Seq(pk))
    val node     = HashSetCasperTestNode.network(Vector(sk), genesis).head
    import node._

    new BlockApproverProtocol[Id](node.validatorId, genesis, requiredSigs) -> node
  }

}
