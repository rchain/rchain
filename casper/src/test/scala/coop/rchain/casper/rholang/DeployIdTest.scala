package coop.rchain.casper.rholang

import cats.effect.Resource
import cats.syntax.all._
import cats.implicits.catsSyntaxApplicativeId
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.syntax._
import coop.rchain.casper.rholang.Resources._
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{GDeployId, Par}
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class DeployIdTest extends AnyFlatSpec with Matchers {
  implicit val log: Log[Task]    = new Log.NOPLog[Task]()
  private val dummyMergeableName = BlockRandomSeed.nonNegativeMergeableTagName("dummy")

  private val runtimeManager: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager[Task]("deploy-id-runtime-manager-test", dummyMergeableName)

  private val sk = ConstructDeploy.defaultSec

  private def deploy(
      deployer: PrivateKey,
      rho: String,
      timestamp: Long = System.currentTimeMillis(),
      shardId: String
  ): Signed[DeployData] = ConstructDeploy.sourceDeploy(
    source = rho,
    timestamp = System.currentTimeMillis(),
    sec = deployer,
    shardId = shardId
  )

  "Deploy id" should "be equal to deploy signature" in {
    val d = deploy(
      sk,
      s"""new return, deployId(`rho:rchain:deployId`) in { return!(*deployId) }""",
      shardId = genesisContext.genesisBlock.shardId
    )
    val result =
      runtimeManager
        .use(
          mgr =>
            for {
              hash <- RuntimeManager.emptyStateHashFixed.pure[Task]
              res  <- mgr.spawnRuntime >>= { _.captureResults(hash, d) }
            } yield res
        )
        .runSyncUnsafe(10.seconds)

    result.size should be(1)
    result.head should be(GDeployId(d.sig): Par)
  }

  val genesisContext = buildGenesis()

  it should "be resolved during normalization" in effectTest {
    val contract = deploy(
      sk,
      s"""contract @"check"(input, ret) = { new deployId(`rho:rchain:deployId`) in { ret!(*input == *deployId) }}""",
      shardId = genesisContext.genesisBlock.shardId
    )
    val contractCall = deploy(
      sk,
      s"""new return, deployId(`rho:rchain:deployId`), ret in { @"check"!(*deployId, *return) }""",
      shardId = genesisContext.genesisBlock.shardId
    )

    TestNode.standaloneEff(genesisContext).use { node =>
      for {
        block <- node.addBlock(contract)
        result <- node.runtimeManager.spawnRuntime >>= {
                   _.captureResults(block.postStateHash, contractCall)
                 }
        _ = assert(result.size == 1)
        _ = assert(result.head == (GBool(false): Par))
      } yield ()
    }
  }
}
