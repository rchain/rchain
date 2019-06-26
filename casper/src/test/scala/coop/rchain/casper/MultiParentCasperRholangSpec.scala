package coop.rchain.casper

import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode.{Effect, _}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.rholang.{RegistrySigGen, RuntimeManager}
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil, RSpaceUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperRholangSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._
  import RSpaceUtil._

  implicit val timeEff: LogicalTime[Effect] = new LogicalTime[Effect]

  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val genesis = buildGenesis(
    buildGenesisParameters(4, createBonds(validatorPks))
  )

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "create blocks based on deploys" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { implicit node =>
      implicit val casper: MultiParentCasperImpl[Effect] = node.casperEff
      implicit val rm: RuntimeManager[Effect]            = node.runtimeManager

      for {
        deploy <- ConstructDeploy.basicDeployData[Effect](0)
        _      <- MultiParentCasper[Effect].deploy(deploy)

        createBlockResult <- MultiParentCasper[Effect].createBlock
        Created(block)    = createBlockResult
        deploys           = block.body.get.deploys.flatMap(_.deploy)
        parents           = ProtoUtil.parentHashes(block)

        _      = parents.size should be(1)
        _      = parents.head should be(genesis.blockHash)
        _      = deploys.size should be(1)
        _      = deploys.head should be(deploy)
        data   <- getDataAtPublicChannel[Effect](block, 0)
        result = data shouldBe Seq("0")
      } yield result
    }
  }

  it should "be able to use the registry" in effectTest {

    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      implicit val rm: RuntimeManager[Effect] = node.runtimeManager

      val registerSource =
        """
          |new uriCh, rr(`rho:registry:insertArbitrary`), hello in {
          |  contract hello(@name, return) = { 
          |    return!("Hello, ${name}!" %% {"name" : name}) 
          |  } |
          |  rr!(bundle+{*hello}, *uriCh)
          |}
        """.stripMargin

      def callSource(registryId: String) =
        s"""
           |new out, rl(`rho:registry:lookup`), helloCh in {
           |  rl!($registryId, *helloCh) |
           |  for(hello <- helloCh){
           |    hello!("World", *out)
           |  }
           |}
         """.stripMargin

      def calculateUnforgeableName(timeStamp: Long): String =
        Base16.encode(
          RegistrySigGen
            .generateUnforgeableNameId(Secp256k1.toPublic(ConstructDeploy.defaultSec), timeStamp)
        )

      for {
        registerDeploy <- ConstructDeploy.sourceDeployNowF(registerSource)
        block0         <- node.addBlock(registerDeploy)
        registryId <- getDataAtPrivateChannel[Effect](
                       block0,
                       calculateUnforgeableName(registerDeploy.timestamp)
                     )
        callDeploy <- ConstructDeploy.sourceDeployNowF(callSource(registryId.head))
        block1     <- node.addBlock(callDeploy)
        data <- getDataAtPrivateChannel[Effect](
                 block1,
                 calculateUnforgeableName(callDeploy.timestamp)
               )
        _ = data shouldBe Seq("\"Hello, World!\"")
      } yield ()
    }
  }
}
