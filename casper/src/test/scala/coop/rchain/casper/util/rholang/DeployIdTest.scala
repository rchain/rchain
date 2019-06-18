package coop.rchain.casper.util.rholang

import cats.effect.Resource
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.rholang.Resources._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.Validator.Validator
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{GDeployId, Par}
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.rholang.interpreter.accounting
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class DeployIdTest extends FlatSpec with Matchers {

  private val runtimeManager: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("deploy-id-runtime-manager-test")

  private val sk = PrivateKey(
    Base16.unsafeDecode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
  )

  private def deploy(
      deployer: PrivateKey,
      rho: String,
      timestamp: Long = System.currentTimeMillis()
  ): DeployData = ConstructDeploy.sourceDeploy(
    source = rho,
    timestamp = System.currentTimeMillis(),
    accounting.MAX_VALUE,
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

  it should "be resolved during normalization" in {
    val captureChannel = "__RETURN_VALUE__"
    val contract = deploy(
      sk,
      s"""contract @"check"(input, ret) = { new deployId(`rho:rchain:deployId`) in { ret!(*input == *deployId) }}"""
    )
    val contractCall = deploy(
      sk,
      s"""new deployId(`rho:rchain:deployId`), ret in {
         |  @"check"!(*deployId, *ret) |
         |  for(isEqual <- ret) {
         |    @"$captureChannel"!(*isEqual)
         |  }
         |}
       """.stripMargin
    )
    val result =
      runtimeManager
        .use { mgr =>
          mgr
            .computeState(mgr.emptyStateHash)(
              Seq(contract),
              BlockData(0L, 0L),
              Map.empty[BlockHash, Validator]
            )
            .flatMap { result =>
              val hash = result._1
              mgr
                .captureResults(
                  hash,
                  contractCall,
                  captureChannel
                )
            }
        }
        .runSyncUnsafe(10.seconds)
    result.size should be(1)
    result.head should be(GBool(false): Par)
  }
}
