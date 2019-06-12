package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil, RSpaceUtil}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperRholangSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._
  import RSpaceUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val genesis = buildGenesis(
    buildGenesisParameters(4, createBonds(validatorPks))
  )

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "create blocks based on deploys" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { implicit node =>
      implicit val casper = node.casperEff
      implicit val rm     = node.runtimeManager

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
    //FIXME fix this, genesis should be OK, name needs to be regenerated - why doesn't it reach that part!?

    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      implicit val rm = node.runtimeManager

      val registerDeploy = ConstructDeploy.sourceDeploy(
        """new uriCh, rr(`rho:registry:insertArbitrary`), hello in {
          |  contract hello(@name, return) = { return!("Hello, ${name}!" %% {"name" : name}) } |
          |  rr!(bundle+{*hello}, *uriCh)
          |}
        """.stripMargin,
        1539788365118L, //fix the timestamp so that `uriCh` is known
        accounting.MAX_VALUE
      )

      for {
        block <- node.addBlock(registerDeploy)
        id    = "rho:id:dwg79f7rkqqsz9458tnh4i6nw3yrnurufihg3zicn9nsrp18o9imwk"
        callDeploy = ConstructDeploy.sourceDeploy(
          s"""new rl(`rho:registry:lookup`), helloCh, out in {
             |  rl!(`$id`, *helloCh) |
             |  for(hello <- helloCh){ hello!("World", *out) }
             |}""".stripMargin,
          1539788365119L,
          accounting.MAX_VALUE
        )
        block2 <- node.addBlock(callDeploy)
        data <- getDataAtPrivateChannel[Effect](
                 block2,
                 "2da67f1ca63808777eba9144a5cac519783afc6070dd08642ac6aa5ed51b98d8"
               )
        _ = data shouldBe Seq("\"Hello, World!\"")
      } yield ()
    }
  }
}
