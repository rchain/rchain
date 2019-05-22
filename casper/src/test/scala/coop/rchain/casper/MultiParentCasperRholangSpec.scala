package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil, RSpaceUtil}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperRholangSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._
  import RSpaceUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
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
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { implicit node =>
      import node.casperEff
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
        _                 <- casperEff.deploy(registerDeploy)
        createBlockResult <- casperEff.createBlock
        Created(block)    = createBlockResult
        blockStatus       <- casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
        id                = "rho:id:h7ezekh7ad65ti3ukk7ij65k3b57dr7osiyhocjddwyg7pyuraw3mq"
        callDeploy = ConstructDeploy.sourceDeploy(
          s"""new rl(`rho:registry:lookup`), helloCh, out in {
             |  rl!(`$id`, *helloCh) |
             |  for(hello <- helloCh){ hello!("World", *out) }
             |}""".stripMargin,
          1539788365119L,
          accounting.MAX_VALUE
        )
        _                  <- casperEff.deploy(callDeploy)
        createBlockResult2 <- casperEff.createBlock
        Created(block2)    = createBlockResult2
        block2Status       <- casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
        _                  = blockStatus shouldBe Valid
        _                  = block2Status shouldBe Valid
        data <- getDataAtPrivateChannel[Effect](
                 block2,
                 "83bbcd6ab7ba905e577e943672ac9e9fbf9917e1be496b2a6620c0821850af93"
               )
        _ = data shouldBe Seq("\"Hello, World!\"")
      } yield ()
    }
  }

  it should "have a working faucet (in testnet)" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node.casperEff

      //val skStr = "6061f3ea36d0419d1e9e23c33bba88ed1435427fa2a8f7300ff210b4e9f18a14"
      val pkStr = "16989775f3f207a717134216816d3c9d97b0bfb8d560b29485f23f6ead435f09"
      val sigStr = "ff2c3c3f1854dd97ae5b569b08d0984e6d68b74ad6cea9eb9cd72e891660edd" +
        "a1c9a4471b31dd6dd1c8f9a8629f88db14188e6c00d8a63b2131e69eda2172009"
      val amount = 157L
      val createWalletCode =
        s"""new
           |  walletCh, rl(`rho:registry:lookup`), SystemInstancesCh, faucetCh,
           |  rs(`rho:registry:insertSigned:ed25519`), uriOut
           |in {
           |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
           |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
           |    @SystemInstancesRegistry!("lookup", "faucet", *faucetCh) |
           |    for(faucet <- faucetCh){ faucet!($amount, "ed25519", "$pkStr", *walletCh) } |
           |    for(@[wallet] <- walletCh){ walletCh!!(wallet) }
           |  } |
           |  rs!(
           |    "$pkStr".hexToBytes(),
           |    (9223372036854775807, bundle-{*walletCh}),
           |    "$sigStr".hexToBytes(),
           |    *uriOut
           |  )
           |}""".stripMargin

      //with the fixed user+timestamp we know that walletCh is registered at `rho:id:mrs88izurkgki71dpjqamzg6tgcjd6sk476c9msks7tumw4a6e39or`
      val createWalletDeploy = ConstructDeploy.sourceDeploy(
        source = createWalletCode,
        timestamp = 1540570144121L,
        phlos = accounting.MAX_VALUE,
        sec = PrivateKey(
          Base16.unsafeDecode("6061f3ea36d0419d1e9e23c33bba88ed1435427fa2a8f7300ff210b4e9f18a14")
        )
      )

      for {
        createBlockResult <- casperEff.deploy(createWalletDeploy) *> casperEff.createBlock
        Created(block)    = createBlockResult
        blockStatus       <- casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
        balanceQuery = ConstructDeploy.sourceDeploy(
          s"""new
             |  rl(`rho:registry:lookup`), walletFeedCh
             |in {
             |  rl!(`rho:id:mrs88izurkgki71dpjqamzg6tgcjd6sk476c9msks7tumw4a6e39or`, *walletFeedCh) |
             |  for(@(_, walletFeed) <- walletFeedCh) {
             |    for(wallet <- @walletFeed) { wallet!("getBalance", "__SCALA__") }
             |  }
             |}""".stripMargin,
          0L,
          accounting.MAX_VALUE,
          sec = PrivateKey(
            Base16.unsafeDecode("6061f3ea36d0419d1e9e23c33bba88ed1435427fa2a8f7300ff210b4e9f18a14")
          )
        )
        newWalletBalance <- node.runtimeManager
                             .captureResults(
                               ProtoUtil.postStateHash(block),
                               balanceQuery
                             )
        _      = blockStatus shouldBe Valid
        result = newWalletBalance.head.exprs.head.getGInt shouldBe amount
      } yield result
    }
  }

}
