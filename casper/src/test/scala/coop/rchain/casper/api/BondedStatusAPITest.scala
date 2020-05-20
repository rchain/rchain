package coop.rchain.casper.api

import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.instances.list._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine.Engine
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.helper._
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.ConstructDeploy.basicDeployData
import coop.rchain.casper.batch2.EngineWithCasper
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Metrics
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.shared.Cell
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

  def bondedStatus(publicKey: PublicKey)(node: TestNode[Effect]): Task[Boolean] = {
    import node.logEff
    val engine = new EngineWithCasper[Task](node.casperEff)
    Cell.mvarCell[Task, Engine[Task]](engine).flatMap { implicit engineCell =>
      BlockAPI
        .bondStatus[Task](ByteString.copyFrom(publicKey.bytes))
        .map(_.right.value)
    }
  }

  "bondStatus" should "return true for bonded validator" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 3).use {
      case n1 +: n2 +: n3 +: _ =>
        (bondedStatus(n1.validatorId.get.publicKey)(n1) shouldBeF true) >>
          (bondedStatus(n2.validatorId.get.publicKey)(n1) shouldBeF true) >>
          (bondedStatus(n3.validatorId.get.publicKey)(n1) shouldBeF true)
    }
  }

  "bondStatus" should "return false for not bonded validators" in effectTest {
    TestNode.standaloneEff(genesisContext).use { node =>
      val (_, publicKey) = Secp256k1.newKeyPair
      bondedStatus(publicKey)(node) shouldBeF false
    }
  }

  "bondStatus" should "return true for newly bonded validator" in effectTest {
    TestNode.networkEff(genesisContext, networkSize = 4).use {
      case nodes @ n1 +: n2 +: n3 +: n4 +: _ =>
        for {
          produceDeploys <- (0 until 3).toList.traverse(i => basicDeployData[Task](i))
          bondDeploy     <- BondingUtil.bondingDeploy[Task](1000, n4.validatorId.get.privateKey)

          _  <- bondedStatus(n4.validatorId.get.publicKey)(n1) shouldBeF false
          b1 <- n1.propagateBlock(bondDeploy)(nodes: _*)
          b2 <- n2.propagateBlock(produceDeploys(0))(nodes: _*)

          // n4 is still not bonded since b1 is not finalized yet
          _ <- bondedStatus(n4.validatorId.get.publicKey)(n1) shouldBeF false

          b3 <- n3.propagateBlock(produceDeploys(1))(nodes: _*)
          b4 <- n1.propagateBlock(produceDeploys(2))(nodes: _*)

          // b1 is now finalized, hence n4 is now bonded
          _ <- bondedStatus(n4.validatorId.get.publicKey)(n1) shouldBeF true
        } yield ()
    }
  }
}
