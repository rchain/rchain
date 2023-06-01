package coop.rchain.casper.batch1

import coop.rchain.casper.blocks.proposer.{Created, NoNewDeploys}
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.helper.{BlockApiFixture, TestNode}
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{EitherValues, Inspectors}
import org.scalatest.matchers.should.Matchers

class MultiParentCasperDeploySpec
    extends AnyFlatSpec
    with Matchers
    with Inspectors
    with BlockApiFixture
    with EitherValues {

  import coop.rchain.casper.util.GenesisBuilder._

  val genesis = buildGenesis()

  it should "not create a block with a repeated deploy" in effectTest {
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {

        deploy             <- ConstructDeploy.basicDeployData[Effect](0, shardId = genesis.genesisBlock.shardId)
        _                  <- node0.propagateBlock(deploy)(node1)
        _                  <- node0.blockDagStorage.addDeploy(deploy)
        createBlockResult2 <- node1.proposeSync.attempt
      } yield createBlockResult2.isLeft shouldBe true
    }
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      for {
        deployData     <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 1)
        r              <- node.createBlock(deployData)
        Created(block) = r
      } yield assert(block.state.deploys.head.isFailed)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
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
        val ex = err.left.value
        ex shouldBe a[RuntimeException]
        ex.getMessage shouldBe s"Phlo price $phloPrice is less than minimum price $minPhloPrice."
      }
    }
  }
}
