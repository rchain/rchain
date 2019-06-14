package coop.rchain.casper.util.rholang

import cats.effect.Resource
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.rholang.Resources._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.Validator.Validator
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{GDeployerAuth, Par}
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.rholang.interpreter.accounting
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.concurrent.duration._

class DeployerAuthTest extends FlatSpec with Matchers {

  private val runtimeManager: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("deployer-auth-runtime-manager-test")

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

  "Deployer authentication" should "be forwarded to the reducer" in {
    val sk = PrivateKey(
      Base16.unsafeDecode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
    )
    val pk             = ByteString.copyFrom(Secp256k1.toPublic(sk).bytes)
    val captureChannel = "__DEPLOYER_AUTH_VALUE__"
    val result =
      runtimeManager
        .use(
          mgr =>
            mgr.captureResults(
              mgr.emptyStateHash,
              deploy(sk, s"""new auth(`rho:deployer:auth`) in { @"$captureChannel"!(*auth) }"""),
              captureChannel
            )
        )
        .runSyncUnsafe(10.seconds)

    result.size should be(1)
    result.head should be(GDeployerAuth(pk): Par)
  }

  it should "make drain vault attacks impossible" in {
    def checkAccessGranted(
        deployer: PrivateKey,
        contractUser: PrivateKey,
        isAccessGranted: Boolean
    ): Assertion = {
      val contract = ConstructDeploy.sourceDeploy(
        source =
          s"""new auth(`rho:deployer:auth`) in { contract @"checkAuth"(input, ret) = { ret!(*input == *auth) }}""",
        timestamp = System.currentTimeMillis(),
        accounting.MAX_VALUE,
        sec = deployer
      )
      val captureChannel = "__RETURN_VALUE__"
      val checkAuth =
        s"""new auth(`rho:deployer:auth`), ret in {
           |@"checkAuth"!(*auth, *ret) |
           |  for(isAuthenticated <- ret) {
           |    @"$captureChannel"!(*isAuthenticated)
           |  }
           |} """.stripMargin
      val checkAuthDeploy = deploy(contractUser, checkAuth)
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
                    checkAuthDeploy,
                    captureChannel
                  )
              }
          }
          .runSyncUnsafe(10.seconds)
      result.size should be(1)
      result.head should be(GBool(isAccessGranted): Par)
    }
    val deployer = PrivateKey(
      Base16.unsafeDecode("0000000000000000000000000000000000000000000000000000000000000000")
    )
    val attacker = PrivateKey(
      Base16.unsafeDecode("1111111111111111111111111111111111111111111111111111111111111111")
    )
    checkAccessGranted(deployer, deployer, isAccessGranted = true)
    checkAccessGranted(deployer, attacker, isAccessGranted = false)
  }
}
