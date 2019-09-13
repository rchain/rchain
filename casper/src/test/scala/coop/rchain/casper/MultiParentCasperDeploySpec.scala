package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode._
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
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
        _        = deployId shouldBe ConstructDeploy.sign(deploy).sig
        _        = logEff.infos.size should be(1)
        result   = logEff.infos.head.contains("Received Deploy") should be(true)
      } yield result
    }
  }

  it should "not allow deploy if deploy is missing signature" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.copy(sig = ByteString.EMPTY)
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(MissingSignature))
    }
  }

  it should "not allow deploy if deploy is missing signature algorithm" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.copy(sigAlgorithm = "")
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(MissingSignatureAlgorithm))
    }
  }

  it should "not allow deploy if deploy is missing user" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.copy(deployer = ByteString.EMPTY)
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(MissingUser))
    }
  }

  it should "not allow deploy if deploy is holding non-existing algorithm" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.copy(sigAlgorithm = "SOME_RANDOME_STUFF")
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(UnknownSignatureAlgorithm("SOME_RANDOME_STUFF")))
    }
  }

  it should "not allow deploy if deploy is incorrectly signed" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy
          .copy(sig = ByteString.copyFrom(correctDeploy.sig.toByteArray.reverse))
        deployResult <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(SignatureVerificationFailed))
    }
  }

  it should "not create a block with a repeated deploy" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    TestNode.networkEff(genesis, networkSize = 2).use { nodes =>
      val List(node0, node1) = nodes.toList
      for {
        deploy             <- ConstructDeploy.basicDeployData[Effect](0)
        _                  <- node0.publishBlock(deploy)(node1)
        casper1            = node1.casperEff
        _                  <- casper1.deploy(deploy)
        createBlockResult2 <- casper1.createBlock
        _                  = createBlockResult2 should be(NoNewDeploys)
      } yield ()
    }
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 1)
        block      <- node.createBlock(deployData)
      } yield assert(block.body.deploys.head.errored)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    TestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.sourceDeployNowF[Effect]("Nil", phloLimit = 100)
        block      <- node.createBlock(deployData)
      } yield assert(!block.body.deploys.head.errored)
    }
  }
}
