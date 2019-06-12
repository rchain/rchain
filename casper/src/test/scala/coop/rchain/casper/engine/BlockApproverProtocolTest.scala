package coop.rchain.casper.engine

import cats.implicits._
import coop.rchain.casper.MultiParentCasperTestUtil
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.protocol._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.transport
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler
import org.scalatest.{FlatSpec, Matchers}

class BlockApproverProtocolTest extends FlatSpec with Matchers {
  import BlockApproverProtocolTest._

  implicit private val scheduler: Scheduler = Scheduler.fixedPool("block-approval-protocol-test", 4)

  "BlockApproverProtocol" should "respond to valid ApprovedBlockCandidates" in {
    val n                          = 8
    val (validatorSk, validatorPk) = Secp256k1.newKeyPair
    val bonds                      = Map(validatorPk -> 10L)
    createProtocol(n, validatorSk, bonds).flatMap {
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
    val (validatorSk, validatorPk) = Secp256k1.newKeyPair
    val bonds                      = Map(validatorPk -> 10L)
    createProtocol(n, validatorSk, bonds).flatMap {
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
      sk: PrivateKey,
      bonds: Map[PublicKey, Long]
  ): Effect[(BlockApproverProtocol, HashSetCasperTestNode[Effect])] = {
    import monix.execution.Scheduler.Implicits.global

    val deployTimestamp = 1L

    val genesis =
      MultiParentCasperTestUtil.buildGenesis(
        Genesis(
          shardId = "BlockApproverProtocolTest",
          timestamp = deployTimestamp,
          proofOfStake = ProofOfStake(
            minimumBond = 0L,
            maximumBond = Long.MaxValue,
            validators = bonds.map(Validator.tupled).toSeq
          ),
          genesisPk = Secp256k1.newKeyPair._2,
          vaults = bonds.toList.map {
            case (pk, stake) =>
              RevAddress.fromPublicKey(pk).map(Vault(_, stake))
          }.flattenOption,
          supply = Long.MaxValue
        )
      )
    HashSetCasperTestNode.networkEff(Vector(sk), genesis).use { nodes =>
      val node = nodes.head
      (new BlockApproverProtocol(
        node.validatorId,
        deployTimestamp,
        bonds,
        1L,
        Long.MaxValue,
        requiredSigs
      ) -> node).pure[Effect]
    }
  }

}
