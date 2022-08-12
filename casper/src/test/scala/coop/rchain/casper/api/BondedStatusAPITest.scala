package coop.rchain.casper.api

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.ConstructDeploy.basicDeployData
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.crypto.PublicKey
import coop.rchain.models.syntax._
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Metrics
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BondedStatusAPITest
    extends AnyFlatSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture {
  // 4 nodes with 3 validators bonded
  val keys              = randomValidatorKeyPairs.take(3).toList :+ ConstructDeploy.defaultKeyPair
  val bonds             = createBonds(keys.map(_._2).take(3))
  val genesisParameters = buildGenesisParameters(keys)(bonds)
  val genesisContext    = buildGenesis(genesisParameters)

  implicit val metricsEff = new Metrics.MetricsNOP[Task]

  def bondedStatus(
      node: TestNode[Effect]
  )(publicKey: PublicKey, block: BlockMessage): Task[Boolean] =
    for {
      blockApi <- createBlockApi(node)
      res <- blockApi
              .bondStatus(ByteString.copyFrom(publicKey.bytes), block.some)
              .map(_.value)
    } yield res

  "bondStatus" should "return true for bonded validator" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 3).use {
      case n1 +: n2 +: n3 +: _ =>
        val gB = genesisContext.genesisBlock
        (bondedStatus(n1)(n1.validatorIdOpt.get.publicKey, gB) shouldBeF true) >>
          (bondedStatus(n1)(n2.validatorIdOpt.get.publicKey, gB) shouldBeF true) >>
          (bondedStatus(n1)(n3.validatorIdOpt.get.publicKey, gB) shouldBeF true)
    }
  }

  "bondStatus" should "return false for not bonded validators" in effectTest {
    TestNode.standaloneEff(genesisContext).use { node =>
      val gB             = genesisContext.genesisBlock
      val (_, publicKey) = Secp256k1.newKeyPair
      bondedStatus(node)(publicKey, gB) shouldBeF false
    }
  }

  "bondStatus" should "return true for newly bonded validator" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 4).use {
      case nodes @ n1 +: _ +: _ +: n4 +: _ =>
        val amount = 1000L
        for {
          addBondingWhiteList <- BondingUtil.addValidatorInBondingWhiteList[Task](
                                  amount,
                                  defaultPosVaultPrivateKey,
                                  n4.validatorIdOpt.get.publicKey.bytes.toHexString,
                                  genesisContext.genesisBlock.shardId
                                )
          bondDeploy <- BondingUtil.bondingDeploy[Task](
                         amount,
                         n4.validatorIdOpt.get.privateKey,
                         shardId = genesisContext.genesisBlock.shardId
                       )

          _  <- bondedStatus(n1)(n4.validatorIdOpt.get.publicKey, genesisContext.genesisBlock) shouldBeF false
          b1 <- n1.propagateBlock(addBondingWhiteList)(nodes: _*)
          b2 <- n1.propagateBlock(bondDeploy)(nodes: _*)
          _  <- bondedStatus(n1)(n4.validatorIdOpt.get.publicKey, b2) shouldBeF true
        } yield ()
    }
  }
}
