package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperDeploySpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  val validatorKeys = defaultValidatorSks
  val genesis       = buildGenesis(buildGenesisParameters())

  "MultiParentCasper" should "accept a deploy and return it's id" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy   <- ConstructDeploy.basicDeployData[Effect](0)
        res      <- MultiParentCasper[Effect].deploy(deploy)
        deployId = res.right.get
        _        = deployId shouldBe ConstructDeploy.sign(deploy).sig.toByteArray
        _        = logEff.infos.size should be(2)
        result   = logEff.infos(1).contains("Received Deploy") should be(true)
      } yield result
    }
  }

  it should "not allow deploy if deploy is missing signature" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.withSig(ByteString.EMPTY)
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(MissingSignature))
    }
  }

  it should "not allow deploy if deploy is missing signature algorithm" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.withSigAlgorithm("")
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(MissingSignatureAlgorithm))
    }
  }

  it should "not allow deploy if deploy is missing user" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.withDeployer(ByteString.EMPTY)
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(MissingUser))
    }
  }

  it should "not allow deploy if deploy is holding non-existing algorithm" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.withSigAlgorithm("SOME_RANDOME_STUFF")
        deployResult    <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(UnknownSignatureAlgorithm("SOME_RANDOME_STUFF")))
    }
  }

  it should "not allow deploy if deploy is incorrectly signed" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      val casper           = node.casperEff
      implicit val timeEff = new LogicalTime[Effect]

      for {
        correctDeploy <- ConstructDeploy.basicDeployData[Effect](0)
        incorrectDeploy = correctDeploy.withSig(
          ByteString.copyFrom(correctDeploy.sig.toByteArray.reverse)
        )
        deployResult <- casper.deploy(incorrectDeploy)
      } yield deployResult should be(Left(SignatureVerificationFailed))
    }
  }

  it should "not create a block with a repeated deploy" in effectTest {
    implicit val timeEff = new LogicalTime[Effect]
    HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis).use { nodes =>
      val List(node0, node1) = nodes.toList
      val casper0            = node0.casperEff
      for {
        deploy             <- ConstructDeploy.basicDeployData[Effect](0)
        _                  <- casper0.deploy(deploy)
        createBlockResult  <- casper0.createBlock
        Created(block)     = createBlockResult
        _                  <- casper0.addBlock(block, ignoreDoppelgangerCheck[Effect])
        _                  <- node1.receive()
        casper1            = node1.casperEff
        _                  <- casper1.deploy(deploy)
        createBlockResult2 <- casper1.createBlock
        _                  = createBlockResult2 should be(NoNewDeploys)
      } yield ()
    }
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.basicDeployData[Effect](0, phlos = 1)
        block      <- node.createBlock(deployData)
      } yield assert(block.body.get.deploys.head.errored)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.basicDeployData[Effect](0, phlos = 100)
        block      <- node.createBlock(deployData)
      } yield assert(!block.body.get.deploys.head.errored)
    }
  }
}
