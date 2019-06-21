package coop.rchain.casper.util.rholang

import cats.effect.Resource
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts.TestUtil
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.rholang.Resources._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.Validator.Validator
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{GDeployerId, Par}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.shared.Time
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.concurrent.duration._

class DeployerIdTest extends FlatSpec with Matchers {

  implicit val timeF: Time[Task] = new LogicalTime[Task]

  private val runtimeManager: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("deployer-id-runtime-manager-test")

  "Deployer id" should "be equal to the deployer's public key" in {
    val captureChannel = "__DEPLOYER_AUTH_VALUE__"
    val result =
      runtimeManager
        .use(
          mgr =>
            mgr.captureResults(
              mgr.emptyStateHash,
              ConstructDeploy.sourceDeployNow(
                s""" new auth(`rho:rchain:deployerId`) in { @"$captureChannel"!(*auth) } """
              ),
              captureChannel
            )
        )
        .runSyncUnsafe(10.seconds)

    result.size should be(1)
    result.head should be(
      GDeployerId(ByteString.copyFrom(Secp256k1.toPublic(ConstructDeploy.defaultSec).bytes)): Par
    )
  }

  it should "make drain vault attacks impossible" in {

    def checkAccessGranted(contractUser: PrivateKey, isAccessGranted: Boolean): Assertion = {

      val authSource =
        s"""
           | contract @"checkAuth"(input, ret) = { 
           |   new auth(`rho:rchain:deployerId`) in { 
           |     ret!(*input == *auth) 
           |   }
           | }
         """.stripMargin

      val captureChannel = "__RETURN_VALUE__"

      val checkAuthSource =
        s"""
           | new auth(`rho:rchain:deployerId`), ret in {
           |   @"checkAuth"!(*auth, *ret) |
           |   for(isAuthenticated <- ret) {
           |     @"$captureChannel"!(*isAuthenticated)
           |   }
           | }
         """.stripMargin

      val result = runtimeManager
        .use { mgr =>
          for {
            genesis     <- TestUtil.genesisSetup(mgr)
            authDeploy  <- ConstructDeploy.sourceDeployNowF(authSource)
            time        <- timeF.currentMillis
            postGenHash = genesis.body.get.state.get.postStateHash
            postAuthHash <- mgr
                             .computeState(postGenHash)(
                               Seq(authDeploy),
                               BlockData(time, 0L),
                               Map.empty[Validator, BlockHash]
                             )
                             .map(_._1)
            checkAuthDeploy <- ConstructDeploy.sourceDeployNowF(checkAuthSource, sec = contractUser)
            result          <- mgr.captureResults(postAuthHash, checkAuthDeploy, captureChannel)
          } yield result
        }
        .runSyncUnsafe(30.seconds)

      result.size should be(1)
      result.head should be(GBool(isAccessGranted): Par)
    }
    val deployer = ConstructDeploy.defaultSec
    val attacker = PrivateKey(
      Base16.unsafeDecode("1111111111111111111111111111111111111111111111111111111111111111")
    )
    checkAccessGranted(deployer, isAccessGranted = true)
    checkAccessGranted(attacker, isAccessGranted = false)
  }
}
