package coop.rchain.casper.engine

import cats.Traverse
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts.Vault
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol._
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.util.GenesisBuilder
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.rholang.interpreter.util.RevAddress
import monix.execution.Scheduler
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.blockImplicits.getRandomBlock

class BlockApproverProtocolTest extends FlatSpec with Matchers {
  import BlockApproverProtocolTest._

  implicit private val scheduler: Scheduler = Scheduler.fixedPool("block-approval-protocol-test", 4)

  "BlockApproverProtocol" should "respond to valid ApprovedBlockCandidates" in {
    createProtocol.flatMap {
      case (approver, node) =>
        val unapproved = createUnapproved(approver.requiredSigs, node.genesis)
        import node._

        for {
          _ <- approver.unapprovedBlockPacketHandler[Effect](node.local, unapproved)

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
    createProtocol.flatMap {
      case (approver, node) =>
        val differentUnapproved1 = createUnapproved(approver.requiredSigs / 2, node.genesis) //wrong number of signatures
        val differentUnapproved2 =
          createUnapproved(approver.requiredSigs, getRandomBlock()) //wrong block
        import node._

        for {
          _ <- approver.unapprovedBlockPacketHandler[Effect](
                node.local,
                differentUnapproved1
              )
          _ <- approver.unapprovedBlockPacketHandler[Effect](
                node.local,
                differentUnapproved2
              )

          _ = node.logEff.warns
            .count(_.contains("Received unexpected genesis block candidate")) should be(2)
          queue <- {
            implicit val network = node.transportLayerEff.testNetworkF
            TestNetwork.peerQueue(node.local)
          }
          result = queue.isEmpty should be(true)
        } yield result
    }
  }

  it should "successfully validate correct candidate" in effectTest {
    createProtocol.flatMap {
      case (approver, node) =>
        val unapproved = createUnapproved(approver.requiredSigs, node.genesis)
        import node._

        for {
          r <- BlockApproverProtocol.validateCandidate[Effect](
                candidate = unapproved.candidate,
                requiredSigs = approver.requiredSigs,
                timestamp = approver.deployTimestamp,
                vaults = approver.vaults,
                bonds = unapproved.candidate.block.body.state.bonds.map {
                  case Bond(validator, stake) => validator -> stake
                }.toMap,
                minimumBond = approver.minimumBond,
                maximumBond = approver.maximumBond,
                epochLength = approver.epochLength,
                quarantineLength = approver.quarantineLength,
                numberOfActiveValidators = approver.numberOfActiveValidators
              )
        } yield r shouldBe Right(())
    }
  }

  it should "reject candidate with incorrect bonds" in effectTest {
    createProtocol.flatMap {
      case (approver, node) =>
        val unapproved = createUnapproved(approver.requiredSigs, node.genesis)
        import node._

        for {
          r <- BlockApproverProtocol.validateCandidate[Effect](
                candidate = unapproved.candidate,
                requiredSigs = approver.requiredSigs,
                timestamp = approver.deployTimestamp,
                vaults = approver.vaults,
                bonds = Map.empty, // bonds are incorrect
                minimumBond = approver.minimumBond,
                maximumBond = approver.maximumBond,
                epochLength = approver.epochLength,
                quarantineLength = approver.quarantineLength,
                numberOfActiveValidators = approver.numberOfActiveValidators
              )
        } yield r shouldBe (Left("Block bonds don't match expected."))
    }
  }

  it should "reject candidate with incorrect vaults" in effectTest {
    createProtocol.flatMap {
      case (approver, node) =>
        val unapproved = createUnapproved(approver.requiredSigs, node.genesis)
        import node._

        for {
          r <- BlockApproverProtocol.validateCandidate[Effect](
                candidate = unapproved.candidate,
                requiredSigs = approver.requiredSigs,
                timestamp = approver.deployTimestamp,
                vaults = Seq.empty[Vault],
                bonds = unapproved.candidate.block.body.state.bonds.map {
                  case Bond(validator, stake) => validator -> stake
                }.toMap,
                minimumBond = approver.minimumBond,
                maximumBond = approver.maximumBond,
                epochLength = approver.epochLength,
                quarantineLength = approver.quarantineLength,
                numberOfActiveValidators = approver.numberOfActiveValidators
              )
        } yield r shouldBe (Left(
          "Mismatch between number of candidate deploys and expected number of deploys."
        ))
    }
  }

  it should "reject candidate with incorrect blessed contracts" in effectTest {
    createProtocol.flatMap {
      case (approver, node) =>
        val unapproved = createUnapproved(approver.requiredSigs, node.genesis)
        import node._

        for {
          r <- BlockApproverProtocol.validateCandidate[Effect](
                candidate = unapproved.candidate,
                requiredSigs = approver.requiredSigs,
                timestamp = approver.deployTimestamp,
                vaults = approver.vaults,
                bonds = unapproved.candidate.block.body.state.bonds.map {
                  case Bond(validator, stake) => validator -> stake
                }.toMap,
                // genesis params are incorrect
                minimumBond = approver.minimumBond + 1,
                maximumBond = approver.maximumBond - 1,
                epochLength = approver.epochLength + 1,
                quarantineLength = approver.quarantineLength + 1,
                numberOfActiveValidators = approver.numberOfActiveValidators + 1
              )
        } yield r.isLeft shouldBe true
    }
  }
}

object BlockApproverProtocolTest {
  def createUnapproved(requiredSigs: Int, block: BlockMessage): UnapprovedBlock =
    UnapprovedBlock(ApprovedBlockCandidate(block, requiredSigs), 0L, 0L)

  def unapprovedToPacket(u: UnapprovedBlock): Packet = ToPacket(u.toProto)

  def createProtocol: Effect[(BlockApproverProtocol, TestNode[Effect])] = {
    import monix.execution.Scheduler.Implicits.global

    val params @ (_, _, genesisParams) = GenesisBuilder.buildGenesisParameters()
    val context                        = GenesisBuilder.buildGenesis(params)

    val bonds        = genesisParams.proofOfStake.validators.map(v => v.pk -> v.stake).toMap
    val requiredSigs = bonds.size - 1

    TestNode.networkEff(context, networkSize = 1).use { nodes =>
      val node = nodes.head
      BlockApproverProtocol
        .of[Effect](
          node.validatorId.get,
          genesisParams.timestamp,
          genesisParams.vaults,
          bonds,
          genesisParams.proofOfStake.minimumBond,
          genesisParams.proofOfStake.maximumBond,
          genesisParams.proofOfStake.epochLength,
          genesisParams.proofOfStake.quarantineLength,
          genesisParams.proofOfStake.numberOfActiveValidators,
          requiredSigs
        )
        .map(_ -> node)
    }
  }

}
