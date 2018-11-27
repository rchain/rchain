package coop.rchain.casper.util.comm

import coop.rchain.casper.HashSetCasperTest
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.{BlockStoreTestFixture, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.transport
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.interpreter.Runtime
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import org.scalatest.{FlatSpec, Matchers}

class BlockApproverProtocolTest extends FlatSpec with Matchers {
  import BlockApproverProtocolTest._

  "BlockApproverProtocol" should "respond to valid ApprovedBlockCandidates" in {
    val n                          = 8
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val bonds                      = Map(validatorPk -> 10L)
    val (approver, node)           = createProtocol(n, Seq.empty, validatorSk, bonds)
    val unapproved                 = createUnapproved(n, node.genesis)
    import node._

    approver.unapprovedBlockPacketHandler(node.local, unapproved).runSyncUnsafe(10.seconds)

    node.logEff.infos.exists(_.contains("Approval sent in response")) should be(true)
    node.logEff.warns.isEmpty should be(true)

    node.transportLayerEff.msgQueues(node.local).get.runSyncUnsafe(10.seconds).size should be(1)
  }

  it should "log a warning for invalid ApprovedBlockCandidates" in {
    val n                          = 8
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val bonds                      = Map(validatorPk -> 10L)
    val (approver, node)           = createProtocol(n, Seq.empty, validatorSk, bonds)
    val differentUnapproved1       = createUnapproved(n / 2, node.genesis) //wrong number of signatures
    val differentUnapproved2       = createUnapproved(n, BlockMessage.defaultInstance) //wrong block
    import node._

    approver
      .unapprovedBlockPacketHandler(node.local, differentUnapproved1)
      .runSyncUnsafe(10.seconds)
    approver
      .unapprovedBlockPacketHandler(node.local, differentUnapproved2)
      .runSyncUnsafe(10.seconds)

    node.logEff.warns.count(_.contains("Received unexpected candidate")) should be(2)

    node.transportLayerEff.msgQueues(node.local).get.runSyncUnsafe(10.seconds).isEmpty should be(
      true
    )
  }
}

object BlockApproverProtocolTest {
  def createUnapproved(requiredSigs: Int, block: BlockMessage): UnapprovedBlock =
    UnapprovedBlock(Some(ApprovedBlockCandidate(Some(block), requiredSigs)), 0L, 0L)

  def unapprovedToPacket(u: UnapprovedBlock): Packet =
    Packet(transport.UnapprovedBlock.id, u.toByteString)

  def createProtocol(
      requiredSigs: Int,
      wallets: Seq[PreWallet],
      sk: Array[Byte],
      bonds: Map[Array[Byte], Long]
  ): (BlockApproverProtocol[Task], HashSetCasperTestNode[Task]) = {
    import monix.execution.Scheduler.Implicits.global

    val runtimeDir = BlockStoreTestFixture.dbDir
    val activeRuntime =
      Runtime.create[Task, Task.Par](runtimeDir, 1024L * 1024).runSyncUnsafe(10.seconds)
    val runtimeManager = RuntimeManager.fromRuntime[Task](activeRuntime).runSyncUnsafe(10.seconds)

    val deployTimestamp = 1L

    val genesis = HashSetCasperTest.buildGenesis(
      wallets,
      bonds,
      1L,
      Long.MaxValue,
      Faucet.noopFaucet,
      deployTimestamp
    )
    val node = HashSetCasperTestNode.network(Vector(sk), genesis).head

    new BlockApproverProtocol(
      node.validatorId,
      deployTimestamp,
      runtimeManager,
      bonds,
      wallets,
      1L,
      Long.MaxValue,
      false,
      requiredSigs
    ) -> node
  }

}
