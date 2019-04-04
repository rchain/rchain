package coop.rchain.casper

import java.nio.file.Files

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, BlockUtil, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{BondingUtil, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.{signBlock, toJustification}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.{transport, CommError, TimeOut}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.{accounting, Runtime}
import coop.rchain.models.{Expr, Par}
import coop.rchain.shared.StoreType
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.eitherT._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Inspectors, Matchers}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.catscontrib.ski.kp2
import coop.rchain.comm.rp.Connect.Connections
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import org.scalatest

import scala.collection.immutable
import scala.util.Random
import scala.concurrent.duration._

class MultiParentCasperDeploySpec extends FlatSpec with Matchers with Inspectors {

  import HashSetCasperTest._

  implicit val timeEff = new LogicalTime[Effect]

  private val (otherSk, otherPk)          = Ed25519.newKeyPair
  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val (ethPivKeys, ethPubKeys)    = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val ethAddresses =
    ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
  private val wallets     = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
  private val bonds       = createBonds(validators)
  private val minimumBond = 100L
  private val genesis =
    buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)

  "MultiParentCasper" should "accept deploys" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deploy <- ConstructDeploy.basicDeployData[Effect](0)
      _      <- MultiParentCasper[Effect].deploy(deploy)

      _      = logEff.infos.size should be(2)
      result = logEff.infos(1).contains("Received Deploy") should be(true)
      _      <- node.tearDown()
    } yield result
  }

  it should "not allow deploy if deploy is missing signature" in effectTest {
    val node             = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val casper           = node.casperEff
    implicit val timeEff = new LogicalTime[Effect]

    for {
      correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
      incorrectDeploy = correctDeploy.withSig(ByteString.EMPTY)
      deployResult    <- casper.deploy(incorrectDeploy)
      _               <- node.tearDown()
    } yield deployResult should be(Left(MissingSignature))
  }

  it should "not allow deploy if deploy is missing signature algorithm" in effectTest {
    val node             = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val casper           = node.casperEff
    implicit val timeEff = new LogicalTime[Effect]

    for {
      correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
      incorrectDeploy = correctDeploy.withSigAlgorithm("")
      deployResult    <- casper.deploy(incorrectDeploy)
      _               <- node.tearDown()
    } yield deployResult should be(Left(MissingSignatureAlgorithm))
  }

  it should "not allow deploy if deploy is missing user" in effectTest {
    val node             = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val casper           = node.casperEff
    implicit val timeEff = new LogicalTime[Effect]

    for {
      correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
      incorrectDeploy = correctDeploy.withDeployer(ByteString.EMPTY)
      deployResult    <- casper.deploy(incorrectDeploy)
      _               <- node.tearDown()
    } yield deployResult should be(Left(MissingUser))
  }

  it should "not allow deploy if deploy is holding non-existing algorithm" in effectTest {
    val node             = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val casper           = node.casperEff
    implicit val timeEff = new LogicalTime[Effect]

    for {
      correctDeploy   <- ConstructDeploy.basicDeployData[Effect](0)
      incorrectDeploy = correctDeploy.withSigAlgorithm("SOME_RANDOME_STUFF")
      deployResult    <- casper.deploy(incorrectDeploy)
      _               <- node.tearDown()
    } yield deployResult should be(Left(UnknownSignatureAlgorithm("SOME_RANDOME_STUFF")))
  }

  it should "not allow deploy if deploy is incorrectly signed" in effectTest {
    val node             = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val casper           = node.casperEff
    implicit val timeEff = new LogicalTime[Effect]

    for {
      correctDeploy <- ConstructDeploy.basicDeployData[Effect](0)
      incorrectDeploy = correctDeploy.withSig(
        ByteString.copyFrom(correctDeploy.sig.toByteArray.reverse)
      )
      deployResult <- casper.deploy(incorrectDeploy)
      _            <- node.tearDown()
    } yield deployResult should be(Left(SignatureVerificationFailed))
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deployData        <- ConstructDeploy.basicDeployData[Effect](0, phlos = 1)
      _                 <- node.casperEff.deploy(deployData)
      createBlockResult <- MultiParentCasper[Effect].createBlock
      Created(block)    = createBlockResult
    } yield assert(block.body.get.deploys.head.errored)
  }

  it should "succeed if given enough phlos for deploy" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deployData <- ConstructDeploy.basicDeployData[Effect](0, phlos = 100)
      _          <- node.casperEff.deploy(deployData)

      createBlockResult <- MultiParentCasper[Effect].createBlock
      Created(block)    = createBlockResult
    } yield assert(!block.body.get.deploys.head.errored)
  }

  it should "allow paying for deploys" in effectTest {
    val node      = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val (sk, pk)  = Ed25519.newKeyPair
    val user      = ByteString.copyFrom(pk)
    val timestamp = System.currentTimeMillis()
    val phloPrice = 1L
    val amount    = 847L
    val sigDeployData = ConstructDeploy
      .sourceDeploy(
        s"""new retCh in { @"blake2b256Hash"!([0, $amount, *retCh].toByteArray(), "__SCALA__") }""",
        timestamp,
        accounting.MAX_VALUE,
        sec = PrivateKey(sk)
      )

    for {
      capturedResults <- node.runtimeManager
                          .captureResults(
                            ProtoUtil.postStateHash(genesis),
                            sigDeployData
                          )
      sigData     = capturedResults.head.exprs.head.getGByteArray
      sig         = Base16.encode(Ed25519.sign(sigData.toByteArray, sk))
      pkStr       = Base16.encode(pk)
      paymentCode = s"""new
         |  paymentForward, walletCh, rl(`rho:registry:lookup`),
         |  SystemInstancesCh, faucetCh, posCh
         |in {
         |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
         |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
         |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
         |    @SystemInstancesRegistry!("lookup", "faucet", *faucetCh) |
         |    for(faucet <- faucetCh; pos <- posCh){
         |      faucet!($amount, "ed25519", "$pkStr", *walletCh) |
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
          sec = PrivateKey(sk)
        )

      paymentQuery = ConstructDeploy
        .sourceDeploy(
          """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
        |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
        |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
        |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
        |    for(pos <- posCh){ pos!("lastPayment", "__SCALA__") }
        |  }
        |}""".stripMargin,
          0L,
          accounting.MAX_VALUE,
          sec = PrivateKey(sk)
        )

      deployQueryResult <- deployAndQuery(
                            node,
                            paymentDeployData,
                            paymentQuery
                          )
      (blockStatus, queryResult) = deployQueryResult
      (codeHashPar, _, userIdPar, timestampPar) = ProtoUtil.getRholangDeployParams(
        paymentDeployData
      )
      phloPurchasedPar = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(phloPrice * amount))))

      _ <- node.tearDown()
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
