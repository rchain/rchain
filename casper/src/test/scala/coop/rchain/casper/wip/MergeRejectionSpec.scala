package coop.rchain.casper.wip

import cats.effect.Sync
import cats.implicits.toBifunctorOps
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper
import coop.rchain.casper.ValidBlock.Valid
import coop.rchain.casper.{PrettyPrinter, ValidBlock}
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance.{Empty, GInt}
import coop.rchain.models.syntax._
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

  val rhoExploreRead = """
    |new return in {
    |  @"READ"!(*return)
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
        "N1 nila"    -> deployNil1a,
        "N1 set 333" -> deploySet1,
        "N1 nilb"    -> deployNil1b
      )
      blocksNode1 <- deploysNode1.values.toList.traverse(nodes(1).addBlock(_))

      // Node 2 change & read
      deploySet2  = deploy(rhoSet(444), defaultSec2)
      deployRead2 = deploy(rhoRead, defaultSec2)
      deploysNode2 = ListMap(
        "N2 set 444" -> deploySet2,
        "N2 read"    -> deployRead2
      )
      // Add deploys in separate blocks or in the same block
      blocksNode2 <- addDeploysNode2(deploysNode2.values.toList)

      // Node 0 accepts all blocks from node 1 and 2
      processingResults <- (blocksNode1 ++ blocksNode2).traverse(nodes(0).processBlock)
      _                 = processingResults.foreach(_ shouldBe Right(Valid))

      // Node 0 creates merged block
      deployNil   = deploy("Nil")
      blockMerged <- nodes(0).addBlock(deployNil)

      // Blocks and deploys for printing
      allBlocks   = blockST +: (blocksNode1 ++ blocksNode2)
      deployNode0 = ListMap(("N0 initial", deployST), ("N0 Nil", deployNil))
      allDeploys  = deployNode0 ++ deploysNode1 ++ deploysNode2

      deployName = (sig: ByteString) => allDeploys.find(x => x._2.sig == sig).get._1

      printBlockMessage = (p: Printer, b: BlockMessage) => {
        p.print(PrettyPrinter.buildString(b, short = true))
          .indent { p =>
            val parents = b.header.parentsHashList
              .map(hash => allBlocks.find(_.blockHash == hash).get)
              .map(PrettyPrinter.buildString(_, short = true))
            p.print("Parents")
              .indent(_.printInline(parents))
          }
          .indent { p =>
            val deploys = b.body.deploys.map(d => deployName(d.deploy.sig))
            p.print("Deploys")
              .indent(_.printInline(deploys))
          }
          .indent { p =>
            val deploys = b.body.rejectedDeploys.map(d => deployName(d.sig))
            p.print("Rejected deploys")
              .indent(_.printInline(deploys))
          }
      }

      // Helper output
      _ = Printer(0)
        .print("Blocks node 1")
        .indent { p =>
          blocksNode1.foreach(printBlockMessage(p, _))
        }
        .print("Blocks node 2")
        .indent { p =>
          blocksNode2.foreach(printBlockMessage(p, _))
        }
        .print("Merged block")
        .indent(printBlockMessage(_, blockMerged))

      printStateResult = (node: TestNode[F], block: BlockMessage) => {
        val rootHash = block.body.state.postStateHash
        for {
          resultPar <- node.runtimeManager.playExploratoryDeploy(rhoExploreRead, rootHash)
          expr      = resultPar.head.exprs.head
          _         = println(s"Contract state on ${PrettyPrinter.buildString(block, short = true)}")
          _         = println(s"  Result: $expr")
        } yield ()
      }

      // Current state of testing contract
      _ <- printStateResult(nodes(1), blocksNode1.last)
      _ <- printStateResult(nodes(2), blocksNode2.last)
      _ <- printStateResult(nodes(0), blockMerged)
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

  // Simple helper to print with indentation
  case class Printer(indentLevel: Int) {
    def print(xs: Iterable[String]): Printer = {
      val indent = Seq.fill(indentLevel * 2)(" ").mkString
      xs.foreach { x =>
        println(s"$indent$x")
      }
      this
    }

    def printInline(xs: Iterable[String]): Printer = {
      val indent = Seq.fill(indentLevel * 2)(" ").mkString
      println(s"$indent[${xs.mkString(", ")}]")
      this
    }

    def print(s: String): Printer = print(Seq(s))

    def indent(f: Printer => Any): Printer = {
      f(Printer(indentLevel + 1))
      this
    }
  }
}
