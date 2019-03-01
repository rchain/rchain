package coop.rchain.casper.util.comm

import coop.rchain.casper.HashSetCasperTest
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Capture._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.transport
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, StoreType}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{FlatSpec, Matchers}

class BlockApproverProtocolTest extends FlatSpec with Matchers {
  import BlockApproverProtocolTest._

  private implicit val scheduler: Scheduler = Scheduler.fixedPool("block-approval-protocol-test", 4)

  "BlockApproverProtocol" should "respond to valid ApprovedBlockCandidates" in {
    val n                          = 8
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val bonds                      = Map(validatorPk -> 10L)
    createProtocol(n, Seq.empty, validatorSk, bonds).flatMap {
      case (approver, node) =>
        val unapproved = createUnapproved(n, node.genesis)
        import node._

        for {
          _ <- approver.unapprovedBlockPacketHandler[Effect](node.local, unapproved, runtimeManager)

          _ = node.logEff.infos.exists(_.contains("Approval sent in response")) should be(true)
          _ = node.logEff.warns.isEmpty should be(true)

          queue <- {
            implicit val network = node.transportLayerEff.testNetworkF
            TestNetwork.peerQueue(node.local)
          }
          result = queue.size should be(1)
        } yield result
    }
  }

  it should "log a warning for invalid ApprovedBlockCandidates" in effectTest {
    val n                          = 8
    val (validatorSk, validatorPk) = Ed25519.newKeyPair
    val bonds                      = Map(validatorPk -> 10L)
    createProtocol(n, Seq.empty, validatorSk, bonds).flatMap {
      case (approver, node) =>
        val differentUnapproved1 = createUnapproved(n / 2, node.genesis)             //wrong number of signatures
        val differentUnapproved2 = createUnapproved(n, BlockMessage.defaultInstance) //wrong block
        import node._

        for {
          _ <- approver.unapprovedBlockPacketHandler[Effect](
                node.local,
                differentUnapproved1,
                runtimeManager
              )
          _ <- approver.unapprovedBlockPacketHandler[Effect](
                node.local,
                differentUnapproved2,
                runtimeManager
              )

          _ = node.logEff.warns.count(_.contains("Received unexpected candidate")) should be(2)
          queue <- {
            implicit val network = node.transportLayerEff.testNetworkF
            TestNetwork.peerQueue(node.local)
          }
          result = queue.isEmpty should be(true)
        } yield result
    }
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
  ): Effect[(BlockApproverProtocol, HashSetCasperTestNode[Effect])] = {
    import monix.execution.Scheduler.Implicits.global

    val runtimeDir                          = BlockDagStorageTestFixture.blockStorageDir
    implicit val log                        = new Log.NOPLog[Task]()
    implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
    val activeRuntime =
      Runtime.create[Task, Task.Par](runtimeDir, 1024L * 1024, StoreType.LMDB).unsafeRunSync
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime).unsafeRunSync

    val deployTimestamp = 1L
    val validators      = bonds.map(b => ProofOfStakeValidator(b._1, b._2)).toSeq

    val genesis = HashSetCasperTest.buildGenesis(
      wallets,
      bonds,
      1L,
      Long.MaxValue,
      Faucet.noopFaucet,
      deployTimestamp
    )
    for {
      nodes <- HashSetCasperTestNode.networkEff(Vector(sk), genesis)
      node  = nodes.head
    } yield
      new BlockApproverProtocol(
        node.validatorId,
        deployTimestamp,
        bonds,
        wallets,
        1L,
        Long.MaxValue,
        false,
        requiredSigs
      ) -> node
  }

}
