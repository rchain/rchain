package coop.rchain.casper.batch1

import cats.implicits.catsSyntaxApplicativeError
import coop.rchain.casper.MultiParentCasper
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.batch2.EngineWithCasper
import coop.rchain.casper.blocks.proposer.{Created, NoNewDeploys}
import coop.rchain.casper.engine.Engine
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Cell
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

  it should "not create a block with a repeated deploy" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        deploy             <- ConstructDeploy.basicDeployData[Effect](0)
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
      } yield assert(block.body.deploys.head.isFailed)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData     <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 100)
        r              <- node.createBlock(deployData)
        Created(block) = r
      } yield assert(!block.body.deploys.head.isFailed)
    }
  }

  it should "reject deploy with phloPrice lower than minPhloPrice" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      import node.logEff
      implicit val noopSpan: Span[Effect] = NoopSpan[Effect]()
      val engine                          = new EngineWithCasper[Effect](node.casperEff)
      Cell.mvarCell[Effect, Engine[Effect]](engine).flatMap { implicit engineCell =>
        val minPhloPrice = 10.toLong
        val phloPrice    = 1.toLong
        for {
          deployData <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloPrice = phloPrice)
          err        <- BlockAPI.deploy[Effect](deployData, None, minPhloPrice = minPhloPrice).attempt
        } yield {
          err.isLeft shouldBe true
          val ex = err.left.get
          ex shouldBe a[RuntimeException]
          ex.getMessage shouldBe s"Phlo price $phloPrice is less than minimum price $minPhloPrice."
        }
      }
    }
  }
}
