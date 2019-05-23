package coop.rchain.casper

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy.sign
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperRholangSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

  implicit val timeEff = new LogicalTime[Effect]

  private val (validatorKeys, validatorPks) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val genesis = buildGenesis(
    buildGenesisParameters(4, createBonds(validatorPks))
  )

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "MultiParentCasper" should "create blocks based on deploys" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      implicit val casper = node.casperEff

      for {
        deploy <- ConstructDeploy.basicDeployData[Effect](0)
        _      <- MultiParentCasper[Effect].deploy(deploy)

        createBlockResult <- MultiParentCasper[Effect].createBlock
        Created(block)    = createBlockResult
        deploys           = block.body.get.deploys.flatMap(_.deploy)
        parents           = ProtoUtil.parentHashes(block)
        storage           <- blockTuplespaceContents(block)

        _      = parents.size should be(1)
        _      = parents.head should be(genesis.blockHash)
        _      = deploys.size should be(1)
        _      = deploys.head should be(deploy)
        result = storage.contains("@{0}!(0)") should be(true)
      } yield result
    }
  }

  it should "be able to use the registry" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node.casperEff

      def now = System.currentTimeMillis()

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
        contents          <- casperEff.storageContents(ProtoUtil.postStateHash(block))
        id = contents
          .split('|')
          .find(
            _.contains(
              //based on the timestamp of registerDeploy, this is uriCh
              "@{Unforgeable(0x61099068fff2d34a6f46c588e8e5ec0e9e131f08e3abe5cd3e3d15289dae7c65)}!"
            )
          )
          .get
          .split('`')(1)
        callDeploy = ConstructDeploy.sourceDeploy(
          s"""new rl(`rho:registry:lookup`), helloCh, out in {
             |  rl!(`$id`, *helloCh) |
             |  for(hello <- helloCh){ hello!("World", *out) }
             |}
      """.stripMargin,
          now,
          accounting.MAX_VALUE
        )
        _                  <- casperEff.deploy(callDeploy)
        createBlockResult2 <- casperEff.createBlock
        Created(block2)    = createBlockResult2
        block2Status       <- casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
        _                  = blockStatus shouldBe Valid
        _                  = block2Status shouldBe Valid
        result <- casperEff
                   .storageContents(ProtoUtil.postStateHash(block2))
                   .map(_.contains("Hello, World!")) shouldBeF true
      } yield result
    }
  }

  it should "have a working faucet (in testnet)" in effectTest {
    HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head).use { node =>
      import node.casperEff

      val sec = PrivateKey(
        Base16.unsafeDecode("0cd69275df1c81ab1487a97c5f8c6e8e091372147261657a2bb9b81e0c53e7b5")
      )
      val pk    = Ed25519.toPublic(sec)
      val pkStr = Base16.encode(pk.bytes)
      val sigStr =
        "a5b9842c858cc918f1cfc48ae5800f61eac63b4fdb35111d3f8e8ffe964ca6b93a850afb2f1151962626842f1db57f9cd812c86b172926bea0a2de85d216ae05"
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

      // with the fixed user+timestamp we know that walletCh is registered at `rho:id:3sc1hdncdeftigmrjw7w7warxmom64o5mociy1fkb5ja55rb7rahww`
      val createWalletDeployData = DeployData(
        deployer = ByteString.copyFrom(pk.bytes),
        timestamp = 1558995791260L,
        term = createWalletCode,
        phloLimit = accounting.MAX_VALUE
      )
      val createWalletDeploy = SignDeployment.sign(sec, createWalletDeployData, Ed25519)

      for {
        createBlockResult <- casperEff.deploy(createWalletDeploy) *> casperEff.createBlock
        Created(block)    = createBlockResult
        blockStatus       <- casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
        balanceQuery = ConstructDeploy.sourceDeploy(
          s"""new
             |  rl(`rho:registry:lookup`), walletFeedCh
             |in {
             |  rl!(`rho:id:3sc1hdncdeftigmrjw7w7warxmom64o5mociy1fkb5ja55rb7rahww`, *walletFeedCh) |
             |  for(@(_, walletFeed) <- walletFeedCh) {
             |    for(wallet <- @walletFeed) { wallet!("getBalance", "__SCALA__") }
             |  }
             |}""".stripMargin,
          0L,
          accounting.MAX_VALUE,
          sec = sec
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
