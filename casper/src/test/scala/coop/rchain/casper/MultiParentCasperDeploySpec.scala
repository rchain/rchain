package coop.rchain.casper

import com.google.protobuf.ByteString
import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.{Expr, Par}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.{accounting, DeployParameters}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperDeploySpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val genesis = buildGenesis(
    buildGenesisParameters(4, createBonds(validatorPks))
  )

  "MultiParentCasper" should "accept deploys" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deploy <- ConstructDeploy.basicDeployData[Effect](0)
        _      <- MultiParentCasper[Effect].deploy(deploy)

        _      = logEff.infos.size should be(2)
        result = logEff.infos(1).contains("Received Deploy") should be(true)
      } yield result
    }
  }

  it should "not allow deploy if deploy is missing signature" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
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
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
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
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
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
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
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
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
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
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData        <- ConstructDeploy.basicDeployData[Effect](0, phlos = 1)
        _                 <- node.casperEff.deploy(deployData)
        createBlockResult <- MultiParentCasper[Effect].createBlock
        Created(block)    = createBlockResult
      } yield assert(block.body.get.deploys.head.errored)
    }
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node._
      implicit val timeEff = new LogicalTime[Effect]

      for {
        deployData <- ConstructDeploy.basicDeployData[Effect](0, phlos = 100)
        _          <- node.casperEff.deploy(deployData)

        createBlockResult <- MultiParentCasper[Effect].createBlock
        Created(block)    = createBlockResult
      } yield assert(!block.body.get.deploys.head.errored)
    }
  }

  it should "allow paying for deploys" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      val (sk, pk)  = Secp256k1.newKeyPair
      val timestamp = System.currentTimeMillis()
      val phloPrice = 1L
      val amount    = 847L
      val sigDeployData = ConstructDeploy
        .sourceDeploy(
          s"""new retCh in { @"blake2b256Hash"!([0, $amount, *retCh].toByteArray(), "__SCALA__") }""",
          timestamp,
          accounting.MAX_VALUE,
          sec = sk
        )

      for {
        capturedResults <- node.runtimeManager
                            .captureResults(
                              ProtoUtil.postStateHash(genesis),
                              sigDeployData
                            )
        sigData     = capturedResults.head.exprs.head.getGByteArray
        sig         = Base16.encode(Secp256k1.sign(sigData.toByteArray, sk))
        pkStr       = Base16.encode(pk.bytes)
        paymentCode = s"""new
           |  paymentForward, walletCh, rl(`rho:registry:lookup`),
           |  SystemInstancesCh, faucetCh, posCh
           |in {
           |  rl!(`rho:lang:systemInstancesRegistry`, *SystemInstancesCh) |
           |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
           |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
           |    @SystemInstancesRegistry!("lookup", "faucet", *faucetCh) |
           |    for(faucet <- faucetCh; pos <- posCh){
           |      faucet!($amount, "secp256k1", "$pkStr", *walletCh) |
           |      for(@[wallet] <- walletCh) {
           |        @wallet!("transfer", $amount, 0, "$sig", *paymentForward, Nil) |
           |        for(@purse <- paymentForward){ pos!("pay", purse, Nil) }
           |      }
           |    }
           |  }
           |}""".stripMargin
        paymentDeployData = ConstructDeploy
          .sourceDeploy(
            paymentCode,
            timestamp,
            accounting.MAX_VALUE,
            phloPrice = phloPrice,
            sec = sk
          )

        paymentQuery = ConstructDeploy
          .sourceDeploy(
            """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
              |  rl!(`rho:lang:systemInstancesRegistry`, *SystemInstancesCh) |
              |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
              |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
              |    for(pos <- posCh){ pos!("lastPayment", "__SCALA__") }
              |  }
              |}""".stripMargin,
            0L,
            accounting.MAX_VALUE,
            sec = sk
          )

        deployQueryResult <- deployAndQuery(
                              node,
                              paymentDeployData,
                              paymentQuery
                            )
        (blockStatus, queryResult) = deployQueryResult
        DeployParameters(codeHashPar, _, userIdPar, timestampPar) = ProtoUtil
          .getRholangDeployParams(
            paymentDeployData
          )
        phloPurchasedPar = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(phloPrice * amount))))
      } yield {
        blockStatus shouldBe Valid

        queryResult.head.exprs.head.getETupleBody.ps match {
          case Seq(
              actualCodeHashPar,
              actualUserIdPar,
              actualTimestampPar,
              actualPhloPurchasedPar
              ) =>
            actualCodeHashPar should be(codeHashPar)
            actualUserIdPar should be(userIdPar)
            actualTimestampPar should be(timestampPar)
            actualPhloPurchasedPar should be(phloPurchasedPar)
        }
      }
    }
  }

}
