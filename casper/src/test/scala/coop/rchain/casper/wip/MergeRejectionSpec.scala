package coop.rchain.casper.wip

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.collection.immutable.ListMap

class MergeRejectionSpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  import ConstructDeploy._

  def deploy(rho: String, sec: PrivateKey = defaultSec) =
    sourceDeployNow(rho, sec = sec)

  val rhoST = """
    |new stCh in {
    |  stCh!(42) |
    |
    |  contract @"SET"(ret, @v) = {
    |    for(@s <- stCh) {
    |      stCh!(v) | ret!(s)
    |    }
    |  } |
    |
    |  contract @"READ"(ret) = {
    |    for(@s <<- stCh) {
    |      ret!(s)
    |    }
    |  }
    |}
    |""".stripMargin

  def rhoSet(num: Int) = s"""
    |new retCh, out(`rho:io:stdout`) in {
    |  out!(("Begin change", $num)) |
    |  @"SET"!(*retCh, $num) |
    |  for(@s <- retCh) {
    |    out!(("Changed to $num, old val", s))
    |  }
    |}
    |""".stripMargin

  val rhoRead = """
    |new retCh, out(`rho:io:stdout`) in {
    |  @"READ"!(*retCh) |
    |  for(@s <- retCh) {
    |    out!(("Read st:", s))
    |  }
    |}
    |""".stripMargin

  def testCase[F[_]: Sync](
      nodes: IndexedSeq[TestNode[F]]
  )(addDeploysNode2: List[Signed[DeployData]] => F[List[BlockMessage]]) = {
    val deployST = deploy(rhoST)

    for {
      // Node 0 creates initial contracts
      blockST <- nodes(0).addBlock(deployST)
      // Node 1 & 2 accepts initial contracts
      _ <- nodes(1).processBlock(blockST)
      _ <- nodes(2).processBlock(blockST)

      // Node 1 change
      deployNil1a = deploy("Nil")
      deploySet1  = deploy(rhoSet(333))
      deployNil1b = deploy("Nil")
      deploysNode1 = ListMap(
        "N1_nila" -> deployNil1a,
        "N1_set"  -> deploySet1,
        "N1_nilb" -> deployNil1b
      )
      blocksNode1 <- deploysNode1.values.toList.traverse(nodes(1).addBlock(_))

      // Node 2 change & read
      deploySet2   = deploy(rhoSet(444), defaultSec2)
      deployRead2  = deploy(rhoRead, defaultSec2)
      deploysNode2 = ListMap("N2_set" -> deploySet2, "N2_read" -> deployRead2)
      // Add deploys in separate blocks or in the same block
      blocksNode2 <- addDeploysNode2(deploysNode2.values.toList)

      // All blocks
      blocks      = blocksNode1 ++ blocksNode2
      blockHashes = blocks.map(_.blockHash)

      // Node 0 accepts all blocks from node 1 and 2
      _ <- blocks.traverse(nodes(0).processBlock)

      // Node 0 creates merged block
      deployNil   = deploy("Nil")
      blockMerged <- nodes(0).addBlock(deployNil)
      // Node 0 blocks
      node0Blocks <- blockHashes.traverse(nodes(0).contains)

      // Node 0 merged parents
      mergedParents = blockHashes.map(blockMerged.header.parentsHashList.contains)

      // Node 0 rejected deploys
      deploys         = deploysNode1 ++ deploysNode2
      rejected        = blockMerged.body.rejectedDeploys.map(_.sig)
      rejectedDeploys = deploys.values.map(_.sig).map(rejected.contains)

      res = ListMap(
        "Merged block    " -> blockMerged,
        "All blocks      " -> node0Blocks,
        "Parents         " -> mergedParents,
        "Deploys         " -> deploys.keys,
        "Rejected deploys" -> rejectedDeploys
      )
      _ = println(s"RESULT:\n${res.mkString("\n")}")
    } yield ()
  }

  it should "reject deploys built on top of rejected deploy (separate blocks)" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      testCase(nodes) { deploys =>
        // Set and read in separate blocks
        deploys.traverse(nodes(2).addBlock(_))
      }
    }
  }

  it should "reject deploys built on top of rejected deploy (same block)" in effectTest {
    TestNode.networkEff(genesis, networkSize = 3).use { nodes =>
      testCase(nodes) { deploys =>
        // Set and read in the same block
        nodes(2).addBlock(deploys: _*).map(List(_))
      }
    }
  }
}
