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
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Metrics
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{EitherValues, FlatSpec, Matchers}

class BondedStatusAPITest
    extends FlatSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture {
  val genesisParameters = buildGenesisParameters(
    defaultValidatorKeyPairs.take(3) :+ ConstructDeploy.defaultKeyPair,
    createBonds(defaultValidatorPks.take(3))
  )
  val genesisContext = buildGenesis(genesisParameters)

  implicit val metricsEff = new Metrics.MetricsNOP[Task]

  def bondedStatus(publicKey: PublicKey, block: BlockMessage)(
      node: TestNode[Effect]
  ): Task[Boolean] = {
    import node.logEff
    implicit val rm  = node.runtimeManager
    implicit val bds = node.blockDagStorage
    implicit val bs  = node.blockStore
    BlockAPI
      .bondStatus[Task](ByteString.copyFrom(publicKey.bytes), block.some)
      .map(_.right.value)
  }

  "bondStatus" should "return true for bonded validator" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 3).use {
      case n1 +: n2 +: n3 +: _ =>
        val gB = genesisContext.genesisBlock
        (bondedStatus(n1.validatorIdOpt.get.publicKey, gB)(n1) shouldBeF true) >>
          (bondedStatus(n2.validatorIdOpt.get.publicKey, gB)(n1) shouldBeF true) >>
          (bondedStatus(n3.validatorIdOpt.get.publicKey, gB)(n1) shouldBeF true)
    }
  }

  "bondStatus" should "return false for not bonded validators" in effectTest {
    TestNode.standaloneEff(genesisContext).use { node =>
      val gB             = genesisContext.genesisBlock
      val (_, publicKey) = Secp256k1.newKeyPair
      bondedStatus(publicKey, gB)(node) shouldBeF false
    }
  }

  "bondStatus" should "return true for newly bonded validator" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 4).use {
      case nodes @ n1 +: n2 +: n3 +: n4 +: _ =>
        for {
          produceDeploys <- (0 until 3).toList.traverse(
                             i =>
                               basicDeployData[Task](
                                 i,
                                 shardId = genesisContext.genesisBlock.shardId
                               )
                           )
          bondDeploy <- BondingUtil.bondingDeploy[Task](
                         1000,
                         n4.validatorIdOpt.get.privateKey,
                         shardId = genesisContext.genesisBlock.shardId
                       )

          _  <- bondedStatus(n4.validatorIdOpt.get.publicKey, genesisContext.genesisBlock)(n1) shouldBeF false
          b1 <- n1.propagateBlock(bondDeploy)(nodes: _*)
          b2 <- n2.propagateBlock(produceDeploys(0))(nodes: _*)

          // n4 is still not bonded since b1 is not finalized yet
          // TODO relying on finalization here is not a good thing, consider adjusting test to just test bonding
          _ <- bondedStatus(n4.validatorIdOpt.get.publicKey, genesisContext.genesisBlock)(n1) shouldBeF false

          b3 <- n3.propagateBlock(produceDeploys(1))(nodes: _*)
          b4 <- n1.propagateBlock(produceDeploys(2))(nodes: _*)

          // b1 is now finalized, hence n4 is now bonded
          _ <- bondedStatus(n4.validatorIdOpt.get.publicKey, b1)(n1) shouldBeF true
        } yield ()
    }
  }
}
