package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperDeploySpec extends FlatSpec with Matchers with Inspectors {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  "MultiParentCasper" should "accept a deploy and return it's id" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy   <- ConstructDeploy.basicDeployData[Effect](0)
        res      <- MultiParentCasper[Effect].deploy(deploy)
        deployId = res.right.get
      } yield deployId shouldBe deploy.sig
    }
  }

  it should "not allow deploy if deploy is missing user" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = Signed(
          correctDeploy.data.copy(deployer = ByteString.EMPTY),
          Secp256k1,
          ConstructDeploy.defaultSec
        )
        deployResult <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(MissingUser))
    }
  }

  it should "not create a block with a repeated deploy" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        deploy             <- ConstructDeploy.basicDeployData[Effect](0)
        _                  <- node0.propagateBlock(deploy)(node1)
        casper1            = node1.casperEff
        _                  <- casper1.deploy(deploy)
        createBlockResult2 <- casper1.createBlock
      } yield (createBlockResult2 should be(NoNewDeploys))
    }
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 1)
        block      <- node.createBlock(deployData)
      } yield assert(block.body.deploys.head.isFailed)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 100)
        block      <- node.createBlock(deployData)
      } yield assert(!block.body.deploys.head.isFailed)
    }
  }
}
