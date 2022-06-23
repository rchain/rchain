package coop.rchain.casper.batch1

import coop.rchain.casper.blocks.proposer.{Created, NoNewDeploys}
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.helper.{BlockApiFixture, TestNode}
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers

class MultiParentCasperDeploySpec
    extends AnyFlatSpec
    with Matchers
    with Inspectors
    with BlockApiFixture {

  import coop.rchain.casper.util.GenesisBuilder._

  implicit val timeEff = new LogicalTime[Effect]

  val genesis = buildGenesis()

  it should "not create a block with a repeated deploy" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        deploy             <- ConstructDeploy.basicDeployData[Effect](0, shardId = genesis.genesisBlock.shardId)
        _                  <- node0.propagateBlock(deploy)(node1)
        createBlockResult2 <- node1.createBlock(deploy)
      } yield (createBlockResult2 should be(NoNewDeploys))
    }
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData     <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 1)
        r              <- node.createBlock(deployData)
        Created(block) = r
      } yield assert(block.state.deploys.head.isFailed)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData     <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 100)
        r              <- node.createBlock(deployData)
        Created(block) = r
      } yield assert(!block.state.deploys.head.isFailed)
    }
  }

  it should "reject deploy with phloPrice lower than minPhloPrice" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val minPhloPrice = node.minPhloPrice
      val phloPrice    = minPhloPrice - 1L
      for {
        deployData <- ConstructDeploy
                       .sourceDeployNowF[Effect](
                         "Nil",
                         phloPrice = phloPrice,
                         shardId = genesis.genesisBlock.shardId
                       )
        blockApi <- createBlockApi(node)
        err      <- blockApi.deploy(deployData).attempt
      } yield {
        err.isLeft shouldBe true
        val ex = err.left.get
        ex shouldBe a[RuntimeException]
        ex.getMessage shouldBe s"Phlo price $phloPrice is less than minimum price $minPhloPrice."
      }
    }
  }
}
