package coop.rchain.casper.util.rholang

import cats.effect.Resource
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.protocol.DeployData
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util.rholang.Resources._
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.PrivateKey
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{GDeployId, Par}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class DeployIdTest extends FlatSpec with Matchers {

  private val runtimeManager: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("deploy-id-runtime-manager-test")

  private val sk = ConstructDeploy.defaultSec

  private def deploy(
      deployer: PrivateKey,
      rho: String,
      timestamp: Long = System.currentTimeMillis()
  ): DeployData = ConstructDeploy.sourceDeploy(
    source = rho,
    timestamp = System.currentTimeMillis(),
    sec = deployer
  )

  "Deploy id" should "be equal to deploy signature" in {
    val captureChannel = "__DEPLOY_ID_VALUE__"
    val d =
      deploy(sk, s"""new deployId(`rho:rchain:deployId`) in { @"$captureChannel"!(*deployId) }""")
    val result =
      runtimeManager
        .use(
          mgr =>
            mgr.captureResults(
              mgr.emptyStateHash,
              d,
              captureChannel
            )
        )
        .runSyncUnsafe(10.seconds)

    result.size should be(1)
    result.head should be(GDeployId(d.sig): Par)
  }

  val genesisContext = buildGenesis()

  it should "be resolved during normalization" in effectTest {
    val captureChannel = "__RETURN_VALUE__"
    val contract = deploy(
      sk,
      s"""contract @"check"(input, ret) = { new deployId(`rho:rchain:deployId`) in { ret!(*input == *deployId) }}"""
    )
    val contractCall = deploy(
      sk,
      s"""new deployId(`rho:rchain:deployId`), ret in { @"check"!(*deployId, "$captureChannel") }"""
    )

    TestNode.standaloneEff(genesisContext).use { node =>
      for {
        block <- node.addBlock(contract)
        result <- node.runtimeManager
                   .captureResults(ProtoUtil.tuplespace(block), contractCall, captureChannel)
        _ = assert(result.size == 1)
        _ = assert(result.head == (GBool(false): Par))
      } yield ()
    }
  }
}
