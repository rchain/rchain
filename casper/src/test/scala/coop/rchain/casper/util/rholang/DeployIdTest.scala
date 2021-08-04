package coop.rchain.casper.util.rholang

import cats.effect.Resource
import cats.implicits.catsSyntaxApplicativeId
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.protocol.DeployData
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util.rholang.Resources._
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{GDeployId, Par}
import coop.rchain.rspace.history.History.emptyRootHash
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class DeployIdTest extends FlatSpec with Matchers {
  implicit val log: Log[Task] = new Log.NOPLog[Task]()

  private val runtimeManager: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager[Task]("deploy-id-runtime-manager-test")

  private val sk = ConstructDeploy.defaultSec

  private def deploy(
      deployer: PrivateKey,
      rho: String,
      timestamp: Long = System.currentTimeMillis()
  ): Signed[DeployData] = ConstructDeploy.sourceDeploy(
    source = rho,
    timestamp = System.currentTimeMillis(),
    sec = deployer
  )

  "Deploy id" should "be equal to deploy signature" in {
    val d = deploy(sk, s"""new return, deployId(`rho:rchain:deployId`) in { return!(*deployId) }""")
    val result =
      runtimeManager
        .use(mgr => mgr.captureResults(emptyRootHash.toByteString, d))
        .runSyncUnsafe(10.seconds)

    result.size should be(1)
    result.head should be(GDeployId(d.sig): Par)
  }

  val genesisContext = buildGenesis()

  it should "be resolved during normalization" in effectTest {
    val contract = deploy(
      sk,
      s"""contract @"check"(input, ret) = { new deployId(`rho:rchain:deployId`) in { ret!(*input == *deployId) }}"""
    )
    val contractCall = deploy(
      sk,
      s"""new return, deployId(`rho:rchain:deployId`), ret in { @"check"!(*deployId, *return) }"""
    )

    TestNode.standaloneEff(genesisContext).use { node =>
      for {
        block <- node.addBlock(contract)
        result <- node.runtimeManager
                   .captureResults(ProtoUtil.postStateHash(block), contractCall)
        _ = assert(result.size == 1)
        _ = assert(result.head == (GBool(false): Par))
      } yield ()
    }
  }
}
