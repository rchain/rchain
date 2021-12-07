//package coop.rchain.casper.util.rholang
//
//import cats.effect.Resource
//import com.google.protobuf.ByteString
//import coop.rchain.casper.helper.TestNode
//import coop.rchain.casper.util.GenesisBuilder.{buildGenesis, buildGenesisParameters}
//import coop.rchain.casper.util.rholang.Resources._
//import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
//import coop.rchain.crypto.PrivateKey
//import coop.rchain.crypto.codec.Base16
//import coop.rchain.crypto.signatures.Secp256k1
//import coop.rchain.models.Expr.ExprInstance.GBool
//import coop.rchain.models.rholang.implicits._
//import coop.rchain.models.{GDeployerId, Par}
//import coop.rchain.p2p.EffectsTestInstances.LogicalTime
//import coop.rchain.shared.Log
//import coop.rchain.shared.scalatestcontrib.effectTest
//import monix.eval.Task
//import monix.execution.Scheduler.Implicits.global
//import org.scalatest.{FlatSpec, Matchers}
//
//class DeployerIdTest extends FlatSpec with Matchers {
//  implicit val time           = new LogicalTime[Task]
//  implicit val log: Log[Task] = new Log.NOPLog[Task]()
//
//  val runtimeManager: Resource[Task, RuntimeManager[Task]] =
//    mkRuntimeManager[Task]("deployer-id-runtime-manager-test")
//
//  "Deployer id" should "be equal to the deployer's public key" in effectTest {
//    val sk = PrivateKey(
//      Base16.unsafeDecode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
//    )
//    val pk = ByteString.copyFrom(Secp256k1.toPublic(sk).bytes)
//    runtimeManager.use { mgr =>
//      for {
//        deploy <- ConstructDeploy.sourceDeployNowF(
//                   s"""new return, auth(`rho:rchain:deployerId`) in { return!(*auth) }""",
//                   sec = sk
//                 )
//        emptyStateHash = RuntimeManager.emptyStateHashFixed
//        result         <- mgr.captureResults(emptyStateHash, deploy)
//        _              = result.size should be(1)
//        _              = result.head should be(GDeployerId(pk): Par)
//      } yield ()
//    }
//  }
//
//  val genesisContext = buildGenesis(buildGenesisParameters())
//
//  it should "make drain vault attacks impossible" in effectTest {
//    val deployer = ConstructDeploy.defaultSec
//    val attacker = ConstructDeploy.defaultSec2
//
//    checkAccessGranted(deployer, deployer, isAccessGranted = true) >>
//      checkAccessGranted(deployer, attacker, isAccessGranted = false)
//  }
//
//  def checkAccessGranted(
//      deployer: PrivateKey,
//      contractUser: PrivateKey,
//      isAccessGranted: Boolean
//  ): Task[Unit] = {
//    val checkDeployerDefinition =
//      s"""
//         |contract @"checkAuth"(input, ret) = {
//         |  new auth(`rho:rchain:deployerId`) in {
//         |    ret!(*input == *auth)
//         |  }
//         |}""".stripMargin
//    val checkDeployerCall =
//      s"""
//         |new return, auth(`rho:rchain:deployerId`), ret in {
//         |  @"checkAuth"!(*auth, *ret) |
//         |  for(isAuthenticated <- ret) {
//         |    return!(*isAuthenticated)
//         |  }
//         |} """.stripMargin
//
//    TestNode.standaloneEff(genesisContext).use { node =>
//      for {
//        contract        <- ConstructDeploy.sourceDeployNowF(checkDeployerDefinition, sec = deployer)
//        block           <- node.addBlock(contract)
//        stateHash       = ProtoUtil.postStateHash(block)
//        checkAuthDeploy <- ConstructDeploy.sourceDeployNowF(checkDeployerCall, sec = contractUser)
//        result          <- node.runtimeManager.captureResults(stateHash, checkAuthDeploy)
//        _               = assert(result.size == 1)
//        _               = assert(result.head == (GBool(isAccessGranted): Par))
//      } yield ()
//    }
//  }
//
//}
