package coop.rchain.casper

import java.nio.file.Files

import cats.Id
import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, BlockUtil, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{BondingUtil, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.{chooseNonConflicting, signBlock, toJustification}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.{transport, CommError}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.{accounting, Runtime}
import coop.rchain.models.{Expr, Par}
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.catscontrib._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Matchers}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.metrics.Metrics

import scala.collection.immutable
import scala.util.Random

class HashSetCasperTest extends FlatSpec with Matchers {

  import HashSetCasperTest._

  implicit val timeEff   = new LogicalTime[Effect]
  implicit val timeEffId = new LogicalTime[Id]

  private val (otherSk, otherPk)          = Ed25519.newKeyPair
  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val (ethPivKeys, ethPubKeys)    = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val ethAddresses =
    ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
  private val wallets     = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
  private val bonds       = createBonds(validators)
  private val minimumBond = 100L
  private val genesis =
    buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)

  private val scheduler = Scheduler.fixedPool("hashset-casper-test", 4)

  def effect(f: Effect[Assertion]): Assertion = f.value.unsafeRunSync(scheduler).right.get

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "HashSetCasper" should "accept deploys" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deploy <- ProtoUtil.basicDeployData[Effect](0)
      _      <- MultiParentCasper[Effect].deploy(deploy)

      _      = logEff.infos.size should be(1)
      result = logEff.infos.head.contains("Received Deploy") should be(true)
      _      = node.tearDown()
    } yield result
  }

  it should "not allow multiple threads to process the same block" in {
    val scheduler = Scheduler.fixedPool("three-threads", 3)
    val node      = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)(scheduler)
    val casper    = node.casperEff

    val testProgram = for {
      deploy <- ProtoUtil.basicDeployData[Effect](0)
      _      <- casper.deploy(deploy)
      block  <- casper.createBlock.map { case Created(block) => block }
      result <- EitherT(
                 Task.racePair(casper.addBlock(block).value, casper.addBlock(block).value).flatMap {
                   case Left((statusA, running)) =>
                     running.join.map((statusA, _).tupled)

                   case Right((running, statusB)) =>
                     running.join.map((_, statusB).tupled)
                 }
               )
    } yield result
    val threadStatuses: (BlockStatus, BlockStatus) =
      testProgram.value.unsafeRunSync(scheduler).right.get

    threadStatuses should matchPattern { case (Processing, Valid) | (Valid, Processing) => }
    node.tearDown()
  }

  it should "create blocks based on deploys" in effect {
    val node            = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    implicit val casper = node.casperEff

    for {
      deploy <- ProtoUtil.basicDeployData[Effect](0)
      _      <- MultiParentCasper[Effect].deploy(deploy)

      createBlockResult <- MultiParentCasper[Effect].createBlock
      Created(block)    = createBlockResult
      deploys           = block.body.get.deploys.flatMap(_.deploy)
      parents           = ProtoUtil.parentHashes(block)
      storage           <- blockTuplespaceContents(block)

      _      = parents.size should be(1)
      _      = parents.head should be(genesis.blockHash)
      _      = deploys.size should be(1)
      _      = deploys.head.raw should be(Some(deploy))
      result = storage.contains("@{0}!(0)") should be(true)
      _      = node.tearDown()
    } yield result
  }

  it should "accept signed blocks" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deploy               <- ProtoUtil.basicDeployData[Effect](0)
      _                    <- MultiParentCasper[Effect].deploy(deploy)
      createBlockResult    <- MultiParentCasper[Effect].createBlock
      Created(signedBlock) = createBlockResult
      _                    <- MultiParentCasper[Effect].addBlock(signedBlock)
      logMessages = List(
        "Received Deploy",
        "Attempting to add Block",
        "Added",
        "Sent Block #1",
        "New fork-choice tip is block"
      )
      _      = logEff.warns.isEmpty should be(true)
      _      = logEff.infos.zip(logMessages).forall { case (a, b) => a.startsWith(b) } should be(true)
      dag    <- MultiParentCasper[Effect].blockDag
      result <- MultiParentCasper[Effect].estimator(dag) shouldBeF IndexedSeq(signedBlock)
      _      = node.tearDown()
    } yield result
  }

  it should "be able to use the registry" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node.casperEff

    def now = System.currentTimeMillis()
    val registerDeploy = ProtoUtil.sourceDeploy(
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
      blockStatus       <- casperEff.addBlock(block)
      id <- casperEff
             .storageContents(ProtoUtil.postStateHash(block))
             .map(
               _.split('|')
                 .find(
                   _.contains(
                     //based on the timestamp of registerDeploy, this is uriCh
                     "@{Unforgeable(0x744dc7e287a955d8f794054ce07fff6efeecec4473a1ebdf26728d93258e3ad6)}!"
                   )
                 )
                 .get
                 .split('`')(1)
             )
      callDeploy = ProtoUtil.sourceDeploy(
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
      block2Status       <- casperEff.addBlock(block2)
      _                  = blockStatus shouldBe Valid
      _                  = block2Status shouldBe Valid
      result <- casperEff
                 .storageContents(ProtoUtil.postStateHash(block2))
                 .map(_.contains("Hello, World!")) shouldBeF true

      _ = node.tearDown()
    } yield result
  }

  it should "be able to create a chain of blocks from different deploys" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._

    val start = System.currentTimeMillis()

    val deployDatas = Vector(
      "contract @\"add\"(@x, @y, ret) = { ret!(x + y) }",
      "new unforgable in { @\"add\"!(5, 7, *unforgable) }"
    ).zipWithIndex.map(s => ProtoUtil.sourceDeploy(s._1, start + s._2, accounting.MAX_VALUE))

    for {
      createBlockResult1 <- MultiParentCasper[Effect].deploy(deployDatas.head) *> MultiParentCasper[
                             Effect
                           ].createBlock
      Created(signedBlock1) = createBlockResult1
      _                     <- MultiParentCasper[Effect].addBlock(signedBlock1)
      createBlockResult2 <- MultiParentCasper[Effect].deploy(deployDatas(1)) *> MultiParentCasper[
                             Effect
                           ].createBlock
      Created(signedBlock2) = createBlockResult2
      _                     <- MultiParentCasper[Effect].addBlock(signedBlock2)
      storage               <- blockTuplespaceContents(signedBlock2)

      _      = logEff.warns should be(Nil)
      _      = ProtoUtil.parentHashes(signedBlock2) should be(Seq(signedBlock1.blockHash))
      dag    <- MultiParentCasper[Effect].blockDag
      _      <- MultiParentCasper[Effect].estimator(dag) shouldBeF IndexedSeq(signedBlock2)
      result = storage.contains("!(12)") should be(true)
      _      = node.tearDown()
    } yield result
  }

  it should "allow multiple deploys in a single block" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._

    val startTime = System.currentTimeMillis()
    val source    = " for(@x <- @0){ @0!(x) } | @0!(0) "
    val deploys = (source #:: source #:: Stream.empty[String]).zipWithIndex
      .map(s => ProtoUtil.sourceDeploy(s._1, startTime + s._2, accounting.MAX_VALUE))
    for {
      _                 <- deploys.traverse_(MultiParentCasper[Effect].deploy(_))
      createBlockResult <- MultiParentCasper[Effect].createBlock
      Created(block)    = createBlockResult
      _                 <- MultiParentCasper[Effect].addBlock(block)
      result            <- MultiParentCasper[Effect].contains(block) shouldBeF true
      _                 = node.tearDown()
    } yield result
  }

  it should "reject unsigned blocks" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      basicDeployData <- ProtoUtil.basicDeployData[Effect](0)
      createBlockResult <- MultiParentCasper[Effect].deploy(basicDeployData) *> MultiParentCasper[
                            Effect
                          ].createBlock
      Created(block) = createBlockResult
      invalidBlock   = block.withSig(ByteString.EMPTY)
      _              <- MultiParentCasper[Effect].addBlock(invalidBlock)

      _ = logEff.warns.head.contains("Ignoring block") should be(true)
      _ = node.tearDownNode()
      result <- validateBlockStore(node) { blockStore =>
                 blockStore.get(block.blockHash) shouldBeF None
               }
    } yield result
  }

  it should "reject blocks not from bonded validators" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, otherSk)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      basicDeployData <- ProtoUtil.basicDeployData[Effect](0)
      createBlockResult <- MultiParentCasper[Effect].deploy(basicDeployData) *> MultiParentCasper[
                            Effect
                          ].createBlock
      Created(signedBlock) = createBlockResult
      _                    <- MultiParentCasper[Effect].addBlock(signedBlock)
      _                    = logEff.warns.head.contains("Ignoring block") should be(true)
      _                    = node.tearDownNode()
      result <- validateBlockStore(node) { blockStore =>
                 blockStore.get(signedBlock.blockHash) shouldBeF None
               }
    } yield result
  }

  it should "propose blocks it adds to peers" in effect {
    for {
      nodes                <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deployData           <- ProtoUtil.basicDeployData[Effect](0)
      createBlockResult    <- nodes(0).casperEff.deploy(deployData) *> nodes(0).casperEff.createBlock
      Created(signedBlock) = createBlockResult
      _                    <- nodes(0).casperEff.addBlock(signedBlock)
      _                    <- nodes(1).receive()
      result               <- nodes(1).casperEff.contains(signedBlock) shouldBeF true
      _                    = nodes.foreach(_.tearDownNode())
      _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
            validateBlockStore(node) { blockStore =>
              blockStore.get(signedBlock.blockHash) shouldBeF Some(signedBlock)
            }(nodes(0).metricEff)
          }
    } yield result
  }

  it should "add a valid block from peer" in effect {
    for {
      nodes                      <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deployData                 <- ProtoUtil.basicDeployData[Effect](1)
      createBlockResult          <- nodes(0).casperEff.deploy(deployData) *> nodes(0).casperEff.createBlock
      Created(signedBlock1Prime) = createBlockResult
      _                          <- nodes(0).casperEff.addBlock(signedBlock1Prime)
      _                          <- nodes(1).receive()
      _                          = nodes(1).logEff.infos.count(_ startsWith "Added") should be(1)
      result                     = nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(0)
      _                          = nodes.foreach(_.tearDownNode())
      _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
            validateBlockStore(node) { blockStore =>
              blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(signedBlock1Prime)
            }(nodes(0).metricEff)
          }
    } yield result
  }

  it should "handle multi-parent blocks correctly" in {
    val nodes = HashSetCasperTestNode.network(validatorKeys.take(2), genesis)
    val deploys = Vector(
      ProtoUtil.basicDeployData[Id](0),
      ProtoUtil.sourceDeploy(
        "@1!(1) | for(@x <- @1){ @1!(x) }",
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      ),
      ProtoUtil.basicDeployData[Id](2)
    )

    val Created(block0) = nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
    val Created(block1) = nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
    nodes(0).casperEff.addBlock(block0)
    nodes(1).casperEff.addBlock(block1)
    nodes(0).receive()
    nodes(1).receive()
    nodes(0).receive()
    nodes(1).receive()

    //multiparent block joining block0 and block1 since they do not conflict
    val Created(multiparentBlock) = nodes(0).casperEff
      .deploy(deploys(2)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(multiparentBlock)
    nodes(1).receive()

    nodes(0).logEff.warns.isEmpty shouldBe true
    nodes(1).logEff.warns.isEmpty shouldBe true
    multiparentBlock.header.get.parentsHashList.size shouldBe 2
    nodes(0).casperEff.contains(multiparentBlock) shouldBe true
    nodes(1).casperEff.contains(multiparentBlock) shouldBe true

    val finalTuplespace =
      nodes(0).casperEff.storageContents(ProtoUtil.postStateHash(multiparentBlock))
    finalTuplespace.contains("@{0}!(0)") shouldBe true
    finalTuplespace.contains("@{1}!(1)") shouldBe true
    finalTuplespace.contains("@{2}!(2)") shouldBe true

    nodes.foreach(_.tearDown())
  }

  it should "not merge blocks that touch the same channel" in {
    val nodes = HashSetCasperTestNode.network(validatorKeys.take(2), genesis)
    val deploys = Vector(
      ProtoUtil.sourceDeploy(
        "@1!(47)",
        timeEff.currentMillis,
        accounting.MAX_VALUE
      ),
      ProtoUtil.sourceDeploy(
        "for(@x <- @1){ @1!(x) }",
        timeEff.currentMillis,
        accounting.MAX_VALUE
      ),
      ProtoUtil.basicDeployData[Id](2)
    )

    val Created(block0) = nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
    val Created(block1) = nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
    nodes(0).casperEff.addBlock(block0)
    nodes(1).casperEff.addBlock(block1)
    nodes(0).receive()
    nodes(1).receive()
    nodes(0).receive()
    nodes(1).receive()

    val Created(singleParentBlock) = nodes(0).casperEff
      .deploy(deploys(2)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(singleParentBlock)
    nodes(1).receive()

    nodes(0).logEff.warns.isEmpty shouldBe true
    nodes(1).logEff.warns.isEmpty shouldBe true
    singleParentBlock.header.get.parentsHashList.size shouldBe 1
    nodes(0).casperEff.contains(singleParentBlock) shouldBe true
    nodes(1).casperEff.contains(singleParentBlock) shouldBe true

    nodes.foreach(_.tearDown())
  }

  it should "not merge blocks that touch the same channel involving joins" in {
    val nodes = HashSetCasperTestNode.network(validatorKeys.take(2), genesis)
    val deploys = Vector(
      ProtoUtil.sourceDeploy(
        "@1!(47)",
        timeEff.currentMillis,
        accounting.MAX_VALUE
      ),
      ProtoUtil.sourceDeploy(
        "for(@x <- @1; @y <- @2){ @1!(x) }",
        timeEff.currentMillis,
        accounting.MAX_VALUE
      ),
      ProtoUtil.basicDeployData[Id](2)
    )

    val Created(block0) = nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
    val Created(block1) = nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
    nodes(0).casperEff.addBlock(block0)
    nodes(1).casperEff.addBlock(block1)
    nodes(0).receive()
    nodes(1).receive()
    nodes(0).receive()
    nodes(1).receive()

    val Created(singleParentBlock) = nodes(0).casperEff
      .deploy(deploys(2)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(singleParentBlock)
    nodes(1).receive()

    nodes(0).logEff.warns.isEmpty shouldBe true
    nodes(1).logEff.warns.isEmpty shouldBe true
    singleParentBlock.header.get.parentsHashList.size shouldBe 1
    nodes(0).casperEff.contains(singleParentBlock) shouldBe true
    nodes(1).casperEff.contains(singleParentBlock) shouldBe true

    nodes.foreach(_.tearDown())
  }

  it should "allow bonding and distribute the joining fee" in {
    val nodes =
      HashSetCasperTestNode.network(
        validatorKeys :+ otherSk,
        genesis,
        storageSize = 1024L * 1024 * 10
      )
    implicit val runtimeManager = nodes(0).runtimeManager
    val pubKey                  = Base16.encode(ethPubKeys.head.bytes.drop(1))
    val secKey                  = ethPivKeys.head.bytes
    val ethAddress              = ethAddresses.head
    val bondKey                 = Base16.encode(otherPk)
    val walletUnlockDeploy =
      RevIssuanceTest.preWalletUnlockDeploy(ethAddress, pubKey, secKey, "unlockOut")
    val bondingForwarderAddress = BondingUtil.bondingForwarderAddress(ethAddress)
    val bondingForwarderDeploy = ProtoUtil.sourceDeploy(
      BondingUtil.bondingForwarderDeploy(bondKey, ethAddress),
      System.currentTimeMillis(),
      accounting.MAX_VALUE
    )
    val transferStatusOut = BondingUtil.transferStatusOut(ethAddress)
    val bondingTransferDeploy =
      RevIssuanceTest.walletTransferDeploy(
        0,
        wallets.head.initRevBalance.toLong,
        bondingForwarderAddress,
        transferStatusOut,
        pubKey,
        secKey
      )

    val Created(block1) = nodes(0).casperEff.deploy(walletUnlockDeploy) *> nodes(0).casperEff
      .deploy(bondingForwarderDeploy) *> nodes(0).casperEff.createBlock
    val block1Status = nodes(0).casperEff.addBlock(block1)
    nodes.foreach(_.receive) //send to all peers

    val Created(block2) = nodes(1).casperEff
      .deploy(bondingTransferDeploy) *> nodes(1).casperEff.createBlock
    val block2Status = nodes(1).casperEff.addBlock(block2)
    nodes.foreach(_.receive)

    val helloWorldDeploy = ProtoUtil.sourceDeploy(
      """new s(`rho:io:stdout`) in { s!("Hello, World!") }""",
      System.currentTimeMillis(),
      accounting.MAX_VALUE
    )
    //new validator does deploy/propose
    val Created(block3) = nodes.last.casperEff
      .deploy(helloWorldDeploy) *> nodes.last.casperEff.createBlock
    val block3Status = nodes.last.casperEff.addBlock(block3)

    //previous validator does deploy/propose
    val Created(block3Prime) = nodes.head.casperEff
      .deploy(helloWorldDeploy) *> nodes.head.casperEff.createBlock
    val block3PrimeStatus = nodes.head.casperEff.addBlock(block3Prime)

    nodes.foreach(_.receive) //all nodes get the blocks

    block1Status shouldBe Valid
    block2Status shouldBe Valid
    block3Status shouldBe Valid
    block3PrimeStatus shouldBe Valid
    nodes.forall(_.logEff.warns.isEmpty) shouldBe true

    val rankedValidatorQuery = ProtoUtil.sourceDeploy(
      """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
      |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
      |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
      |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
      |    for(pos <- posCh){
      |      new bondsCh, getRanking in {
      |        contract getRanking(@bonds, @acc, return) = {
      |          match bonds {
      |            {key:(stake, _, _, index) ...rest} => {
      |              getRanking!(rest, acc ++ [(key, stake, index)], *return)
      |            }
      |            _ => { return!(acc) }
      |          }
      |        } |
      |        pos!("getBonds", *bondsCh) | for(@bonds <- bondsCh) {
      |          getRanking!(bonds, [], "__SCALA__")
      |        }
      |      }
      |    }
      |  }
      |}""".stripMargin,
      0L,
      accounting.MAX_VALUE
    )
    val validatorBondsAndRanks: Seq[(ByteString, Long, Int)] = runtimeManager
      .captureResults(
        ProtoUtil.postStateHash(block1),
        ProtoUtil.deployDataToDeploy(rankedValidatorQuery)
      )
      .head
      .exprs
      .head
      .getEListBody
      .ps
      .map(
        _.exprs.head.getETupleBody.ps match {
          case Seq(a, b, c) =>
            (a.exprs.head.getGByteArray, b.exprs.head.getGInt, c.exprs.head.getGInt.toInt)
        }
      )

    val joiningFee = minimumBond
    val n          = validatorBondsAndRanks.size
    val joiningFeeDistribution = (1 to n).map { k =>
      k -> ((2L * minimumBond * (n + 1 - k)) / (n * (n + 1)))
    }.toMap
    val total = joiningFeeDistribution.values.sum
    val finalFeesDist =
      joiningFeeDistribution.updated(1, joiningFeeDistribution(1) + joiningFee - total)
    val correctBonds = validatorBondsAndRanks.map {
      case (key, stake, rank) =>
        Bond(key, stake + finalFeesDist(rank))
    }.toSet + Bond(
      ByteString.copyFrom(otherPk),
      wallets.head.initRevBalance.toLong - joiningFee
    )

    val newBonds = block2.getBody.getState.bonds
    newBonds.toSet shouldBe correctBonds

    nodes.foreach(_.tearDown())
  }

  it should "have a working faucet (in testnet)" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node.casperEff

    //val skStr = "6061f3ea36d0419d1e9e23c33bba88ed1435427fa2a8f7300ff210b4e9f18a14"
    val pkStr = "16989775f3f207a717134216816d3c9d97b0bfb8d560b29485f23f6ead435f09"
    val sigStr = "51c2b091559745d51c7270189911d9d894d538f76150ed67d164705dcf0af52" +
      "e101fa06396db2b2ac21a4bfbe3461567b5f8b3d2e666c377cb92d96bc38e2c08"
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
    val createWalletDeploy = ProtoUtil
      .sourceDeploy(createWalletCode, System.currentTimeMillis(), accounting.MAX_VALUE)
      .withTimestamp(1540570144121L)
      .withUser(ProtoUtil.stringToByteString(pkStr))

    val Created(block) = casperEff.deploy(createWalletDeploy) *> casperEff.createBlock
    val blockStatus    = casperEff.addBlock(block)

    val balanceQuery = ProtoUtil.sourceDeploy(
      s"""new
         |  rl(`rho:registry:lookup`), walletFeedCh
         |in {
         |  rl!(`rho:id:mrs88izurkgki71dpjqamzg6tgcjd6sk476c9msks7tumw4a6e39or`, *walletFeedCh) |
         |  for(@(_, walletFeed) <- walletFeedCh) {
         |    for(wallet <- @walletFeed) { wallet!("getBalance", "__SCALA__") }
         |  }
         |}""".stripMargin,
      0L,
      accounting.MAX_VALUE
    )
    val newWalletBalance =
      node.runtimeManager.captureResults(
        ProtoUtil.postStateHash(block),
        ProtoUtil.deployDataToDeploy(balanceQuery)
      )

    blockStatus shouldBe Valid
    newWalletBalance.head.exprs.head.getGInt shouldBe amount

    node.tearDown()
  }

  it should "allow bonding via the faucet" in effect {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node.casperEff

    implicit val runtimeManager = node.runtimeManager
    val (sk, pk)                = Ed25519.newKeyPair
    val pkStr                   = Base16.encode(pk)
    val amount                  = 314L
    val forwardCode             = BondingUtil.bondingForwarderDeploy(pkStr, pkStr)
    for {
      bondingCode <- BondingUtil.faucetBondDeploy[Effect](amount, "ed25519", pkStr, sk)
      forwardDeploy = ProtoUtil.sourceDeploy(
        forwardCode,
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      bondingDeploy = ProtoUtil.sourceDeploy(
        bondingCode,
        forwardDeploy.timestamp + 1,
        accounting.MAX_VALUE
      )
      createBlockResult1 <- casperEff.deploy(forwardDeploy) *> casperEff.createBlock
      Created(block1)    = createBlockResult1
      block1Status       <- casperEff.addBlock(block1)
      createBlockResult2 <- casperEff.deploy(bondingDeploy) *> casperEff.createBlock
      Created(block2)    = createBlockResult2
      block2Status       <- casperEff.addBlock(block2)
      oldBonds           = block1.getBody.getState.bonds
      newBonds           = block2.getBody.getState.bonds
      _                  = block1Status shouldBe Valid
      _                  = block2Status shouldBe Valid
      result             = (oldBonds.size + 1) shouldBe newBonds.size

      _ = node.tearDown()
    } yield result
  }

  it should "not fail if the forkchoice changes after a bonding event" in {
    val localValidators = validatorKeys.take(3)
    val localBonds      = localValidators.map(Ed25519.toPublic).zip(List(10L, 30L, 5000L)).toMap
    val localGenesis =
      buildGenesis(Nil, localBonds, 1L, Long.MaxValue, Faucet.basicWalletFaucet, 0L)
    for {
      nodes <- HashSetCasperTestNode.networkEff(localValidators, localGenesis)

      rm          = nodes.head.runtimeManager
      (sk, pk)    = Ed25519.newKeyPair
      pkStr       = Base16.encode(pk)
      forwardCode = BondingUtil.bondingForwarderDeploy(pkStr, pkStr)
      bondingCode <- BondingUtil.faucetBondDeploy[Effect](50, "ed25519", pkStr, sk)(
                      Sync[Effect],
                      rm,
                      scheduler
                    )
      forwardDeploy = ProtoUtil.sourceDeploy(
        forwardCode,
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      bondingDeploy = ProtoUtil.sourceDeploy(
        bondingCode,
        forwardDeploy.timestamp + 1,
        accounting.MAX_VALUE
      )

      _                    <- nodes.head.casperEff.deploy(forwardDeploy)
      _                    <- nodes.head.casperEff.deploy(bondingDeploy)
      createBlockResult1   <- nodes.head.casperEff.createBlock
      Created(bondedBlock) = createBlockResult1

      bondedBlockStatus = nodes.head.casperEff.addBlock(bondedBlock)
      _                 <- nodes(1).receive()
      _                 <- nodes.head.receive()
      _                 <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses bonding

      createBlockResult2 <- {
        val n = nodes(1)
        import n.casperEff._
        (ProtoUtil.basicDeployData[Effect](0) >>= deploy) *> createBlock
      }
      Created(block2) = createBlockResult2
      status2         <- nodes(1).casperEff.addBlock(block2)
      _               <- nodes.head.receive()
      _               <- nodes(1).receive()
      _               <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses block built on bonding

      createBlockResult3 <- { //nodes(2) proposes a block
        val n = nodes(2)
        import n.casperEff._
        (ProtoUtil.basicDeployData[Effect](1) >>= deploy) *> createBlock
      }
      Created(block3) = createBlockResult3
      status3         <- nodes(2).casperEff.addBlock(block3)
      _               <- nodes.toList.traverse_(_.receive())
      //Since weight of nodes(2) is higher than nodes(0) and nodes(1)
      //their fork-choice changes, thus the new validator
      //is no longer bonded

      createBlockResult4 <- { //nodes(0) proposes a new block
        val n = nodes.head
        import n.casperEff._
        (ProtoUtil.basicDeployData[Effect](2) >>= deploy) *> createBlock
      }
      Created(block4) = createBlockResult4
      status4         <- nodes.head.casperEff.addBlock(block4)
      _               <- nodes.toList.traverse_(_.receive())

      _      = bondedBlockStatus shouldBe Valid
      _      = status2 shouldBe Valid
      _      = status3 shouldBe Valid
      result = status4 shouldBe Valid
      _      = nodes.foreach(_.logEff.warns shouldBe Nil)

      _ = nodes.foreach(_.tearDown())
    } yield result
  }

  it should "allow paying for deploys" in {
    val node      = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    val (sk, pk)  = Ed25519.newKeyPair
    val user      = ByteString.copyFrom(pk)
    val timestamp = System.currentTimeMillis()
    val phloPrice = 1L
    val amount    = 847L
    val sigDeployData = ProtoUtil
      .sourceDeploy(
        s"""new retCh in { @"blake2b256Hash"!([0, $amount, *retCh].toByteArray(), "__SCALA__") }""",
        timestamp,
        accounting.MAX_VALUE
      )
      .withUser(user)
    val sigData = node.runtimeManager
      .captureResults(
        ProtoUtil.postStateHash(genesis),
        ProtoUtil.deployDataToDeploy(sigDeployData)
      )
      .head
      .exprs
      .head
      .getGByteArray
    val sig   = Base16.encode(Ed25519.sign(sigData.toByteArray, sk))
    val pkStr = Base16.encode(pk)
    val paymentCode =
      s"""new
         |  paymentForward, walletCh, rl(`rho:registry:lookup`),
         |  SystemInstancesCh, faucetCh, posCh
         |in {
         |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
         |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
         |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
         |    @SystemInstancesRegistry!("lookup", "faucet", *faucetCh) |
         |    for(faucet <- faucetCh; pos <- posCh){
         |      faucet!($amount, "ed25519", "$pkStr", *walletCh) |
         |      for(@[wallet] <- walletCh) {
         |        @wallet!("transfer", $amount, 0, "$sig", *paymentForward, Nil) |
         |        for(@purse <- paymentForward){ pos!("pay", purse, Nil) }
         |      }
         |    }
         |  }
         |}""".stripMargin
    val paymentDeployData = ProtoUtil
      .sourceDeploy(paymentCode, timestamp, accounting.MAX_VALUE)
      .withPhloPrice(phloPrice)
      .withUser(user)

    val paymentQuery = ProtoUtil.sourceDeploy(
      """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
        |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
        |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
        |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
        |    for(pos <- posCh){ pos!("lastPayment", "__SCALA__") }
        |  }
        |}""".stripMargin,
      0L,
      accounting.MAX_VALUE
    )

    val (blockStatus, queryResult) =
      deployAndQuery(node, paymentDeployData, ProtoUtil.deployDataToDeploy(paymentQuery))

    val (codeHashPar, _, userIdPar, timestampPar) =
      ProtoUtil.getRholangDeployParams(paymentDeployData)
    val phloPurchasedPar = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(phloPrice * amount))))

    blockStatus shouldBe Valid
    queryResult.head.exprs.head.getETupleBody.ps shouldBe Seq(
      codeHashPar,
      userIdPar,
      timestampPar,
      phloPurchasedPar
    )

    node.tearDown()
  }

  it should "reject addBlock when there exist deploy by the same (user, millisecond timestamp) in the chain" in effect {
    for {
      nodes <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deployDatas <- (0 to 2).toList
                      .traverse[Effect, DeployData](i => ProtoUtil.basicDeployData[Effect](i))
      deployPrim0 = deployDatas(1)
        .withTimestamp(deployDatas(0).timestamp)
        .withUser(deployDatas(0).user) // deployPrim0 has the same (user, millisecond timestamp) with deployDatas(0)
      createBlockResult1 <- nodes(0).casperEff
                             .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
      Created(signedBlock1) = createBlockResult1
      _                     <- nodes(0).casperEff.addBlock(signedBlock1)
      _                     <- nodes(1).receive() // receive block1

      createBlockResult2 <- nodes(0).casperEff
                             .deploy(deployDatas(1)) *> nodes(0).casperEff.createBlock
      Created(signedBlock2) = createBlockResult2
      _                     <- nodes(0).casperEff.addBlock(signedBlock2)
      _                     <- nodes(1).receive() // receive block2

      createBlockResult3 <- nodes(0).casperEff
                             .deploy(deployDatas(2)) *> nodes(0).casperEff.createBlock
      Created(signedBlock3) = createBlockResult3
      _                     <- nodes(0).casperEff.addBlock(signedBlock3)
      _                     <- nodes(1).receive() // receive block3

      _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true

      createBlockResult4 <- nodes(1).casperEff
                             .deploy(deployPrim0) *> nodes(1).casperEff.createBlock
      Created(signedBlock4) = createBlockResult4
      _                     <- nodes(1).casperEff.addBlock(signedBlock4) // should succeed
      _                     <- nodes(0).receive() // still receive signedBlock4

      result <- nodes(1).casperEff
                 .contains(signedBlock4) shouldBeF true // Invalid blocks are still added
      // TODO: Fix with https://rchain.atlassian.net/browse/RHOL-1048
      // nodes(0).casperEff.contains(signedBlock4) should be(false)
      //
      // nodes(0).logEff.warns
      //   .count(_ contains "found deploy by the same (user, millisecond timestamp) produced") should be(
      //   1
      // )
      _ = nodes.foreach(_.tearDownNode())

      _ = nodes.toList.traverse_[Effect, Assertion] { node =>
        validateBlockStore(node) { blockStore =>
          for {
            _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
            _      <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
            result <- blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
          } yield result
        }(nodes(0).metricEff)
      }
    } yield result
  }

  it should "ask peers for blocks it is missing" in {
    for {
      nodes <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis)
      deployDatas = Vector(
        "for(_ <- @1){ Nil } | @1!(1)",
        "@2!(2)"
      ).zipWithIndex
        .map(
          d => ProtoUtil.sourceDeploy(d._1, System.currentTimeMillis() + d._2, accounting.MAX_VALUE)
        )

      createBlockResult1 <- nodes(0).casperEff
                             .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
      Created(signedBlock1) = createBlockResult1

      _ <- nodes(0).casperEff.addBlock(signedBlock1)
      _ <- nodes(1).receive()
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block

      createBlockResult2 <- nodes(0).casperEff
                             .deploy(deployDatas(1)) *> nodes(0).casperEff.createBlock
      Created(signedBlock2) = createBlockResult2

      _ <- nodes(0).casperEff.addBlock(signedBlock2)
      _ <- nodes(1).receive() //receives block2
      _ <- nodes(2).receive() //receives block2; asks for block1
      _ <- nodes(1).receive() //receives request for block1; sends block1
      _ <- nodes(2).receive() //receives block1; adds both block1 and block2

      _ <- nodes(2).casperEff.contains(signedBlock1) shouldBeF true
      _ <- nodes(2).casperEff.contains(signedBlock2) shouldBeF true

      _ = nodes(2).logEff.infos
        .count(_ startsWith "Requested missing block") should be(1)
      result = nodes(1).logEff.infos.count(
        s => (s startsWith "Received request for block") && (s endsWith "Response sent.")
      ) should be(1)

      _ = nodes.foreach(_.tearDownNode())
      _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
            validateBlockStore(node) { blockStore =>
              for {
                _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
                result <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
              } yield result
            }(nodes(0).metricEff)
          }
    } yield result
  }

  it should "ignore adding equivocation blocks" in {
    for {
      nodes <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)

      // Creates a pair that constitutes equivocation blocks
      basicDeployData0 <- ProtoUtil.basicDeployData[Effect](0)
      createBlockResult1 <- nodes(0).casperEff
                             .deploy(basicDeployData0) *> nodes(0).casperEff.createBlock
      Created(signedBlock1) = createBlockResult1
      basicDeployData1      <- ProtoUtil.basicDeployData[Effect](1)
      createBlockResult1Prime <- nodes(0).casperEff
                                  .deploy(basicDeployData1) *> nodes(0).casperEff.createBlock
      Created(signedBlock1Prime) = createBlockResult1Prime

      _ <- nodes(0).casperEff.addBlock(signedBlock1)
      _ <- nodes(1).receive()
      _ <- nodes(0).casperEff.addBlock(signedBlock1Prime)
      _ <- nodes(1).receive()

      _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
      result <- nodes(1).casperEff
                 .contains(signedBlock1Prime) shouldBeF false // we still add the equivocation pair

      _ = nodes(0).tearDownNode()
      _ = nodes(1).tearDownNode()
      _ <- validateBlockStore(nodes(1)) { blockStore =>
            for {
              _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
              result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF None
            } yield result
          }(nodes(0).metricEff)
    } yield result
  }

  // See [[/docs/casper/images/minimal_equivocation_neglect.png]] but cross out genesis block
  it should "not ignore equivocation blocks that are required for parents of proper nodes" in {
    for {
      nodes       <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis)
      deployDatas <- (0 to 5).toList.traverse[Effect, DeployData](ProtoUtil.basicDeployData[Effect])

      // Creates a pair that constitutes equivocation blocks
      createBlockResult1 <- nodes(0).casperEff
                             .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
      Created(signedBlock1) = createBlockResult1
      createBlockResult1Prime <- nodes(0).casperEff
                                  .deploy(deployDatas(1)) *> nodes(0).casperEff.createBlock
      Created(signedBlock1Prime) = createBlockResult1Prime

      _ <- nodes(1).casperEff.addBlock(signedBlock1)
      _ <- nodes(0).transportLayerEff.clear(nodes(0).local) //nodes(0) misses this block
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block

      _ <- nodes(0).casperEff.addBlock(signedBlock1Prime)
      _ <- nodes(2).receive()
      _ <- nodes(1).transportLayerEff.clear(nodes(1).local) //nodes(1) misses this block

      _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
      _ <- nodes(2).casperEff.contains(signedBlock1) shouldBeF false

      _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF false
      _ <- nodes(2).casperEff.contains(signedBlock1Prime) shouldBeF true

      createBlockResult2 <- nodes(1).casperEff
                             .deploy(deployDatas(2)) *> nodes(1).casperEff.createBlock
      Created(signedBlock2) = createBlockResult2
      createBlockResult3 <- nodes(2).casperEff
                             .deploy(deployDatas(3)) *> nodes(2).casperEff.createBlock
      Created(signedBlock3) = createBlockResult3

      _ <- nodes(2).casperEff.addBlock(signedBlock3)
      _ <- nodes(1).casperEff.addBlock(signedBlock2)
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) ignores block2
      _ <- nodes(1).receive() // receives block3; asks for block1'
      _ <- nodes(2).receive() // receives request for block1'; sends block1'
      _ <- nodes(1).receive() // receives block1'; adds both block3 and block1'

      _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true
      _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

      createBlockResult4 <- nodes(1).casperEff
                             .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
      Created(signedBlock4) = createBlockResult4
      _                     <- nodes(1).casperEff.addBlock(signedBlock4)

      // Node 1 should contain both blocks constituting the equivocation
      _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
      _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

      _ <- nodes(1).casperEff
            .contains(signedBlock4) shouldBeF true // However, in invalidBlockTracker

      _ = nodes(1).logEff.infos.count(_ startsWith "Added admissible equivocation") should be(1)
      _ = nodes(2).logEff.warns.size should be(0)
      _ = nodes(1).logEff.warns.size should be(1)
      _ = nodes(0).logEff.warns.size should be(0)

      _ = nodes(1).casperEff.normalizedInitialFault(ProtoUtil.weightMap(genesis)) should be(
        1f / (1f + 3f + 5f + 7f)
      )
      _ = nodes.foreach(_.tearDownNode())

      _ <- validateBlockStore(nodes(0)) { blockStore =>
            for {
              _ <- blockStore.get(signedBlock1.blockHash) shouldBeF None
              result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
                         signedBlock1Prime
                       )
            } yield result
          }(nodes(0).metricEff)
      _ <- validateBlockStore(nodes(1)) { blockStore =>
            for {
              _      <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
              result <- blockStore.get(signedBlock4.blockHash) shouldBeF Some(signedBlock4)
            } yield result
          }(nodes(1).metricEff)
      result <- validateBlockStore(nodes(2)) { blockStore =>
                 for {
                   _ <- blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
                   result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
                              signedBlock1Prime
                            )
                 } yield result
               }(nodes(2).metricEff)
    } yield result
  }

  it should "prepare to slash an block that includes a invalid block pointer" in {
    val nodes           = HashSetCasperTestNode.network(validatorKeys.take(3), genesis)
    val deploys         = (0 to 5).map(i => ProtoUtil.basicDeploy[Id](i))
    val deploysWithCost = deploys.map(d => ProcessedDeploy(deploy = Some(d)))

    val Created(signedBlock) = nodes(0).casperEff
      .deploy(deploys(0).raw.get) *> nodes(0).casperEff.createBlock
    val signedInvalidBlock =
      BlockUtil.resignBlock(signedBlock.withSeqNum(-2), nodes(0).validatorId.privateKey) // Invalid seq num

    val blockWithInvalidJustification =
      buildBlockWithInvalidJustification(nodes, deploysWithCost, signedInvalidBlock)

    nodes(1).casperEff.addBlock(blockWithInvalidJustification)
    nodes(0).transportLayerEff
      .clear(nodes(0).local) // nodes(0) rejects normal adding process for blockThatPointsToInvalidBlock

    val signedInvalidBlockPacketMessage =
      packet(nodes(1).local, transport.BlockMessage, signedInvalidBlock.toByteString)
    nodes(0).transportLayerEff.send(nodes(1).local, signedInvalidBlockPacketMessage)
    nodes(1).receive() // receives signedInvalidBlock; attempts to add both blocks

    nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(1)
    nodes.foreach(_.tearDown())
  }

  it should "handle a long chain of block requests appropriately" in {
    val nodes =
      HashSetCasperTestNode.network(validatorKeys.take(2), genesis, storageSize = 1024L * 1024 * 10)

    (0 to 9).foreach { i =>
      val deploy         = ProtoUtil.basicDeployData[Id](i)
      val Created(block) = nodes(0).casperEff.deploy(deploy) *> nodes(0).casperEff.createBlock

      nodes(0).casperEff.addBlock(block)
      nodes(1).transportLayerEff.clear(nodes(1).local) //nodes(1) misses this block
    }
    val Created(block11) = nodes(0).casperEff
      .deploy(ProtoUtil.basicDeployData[Id](10)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block11)

    // Cycle of requesting and passing blocks until block #3 from nodes(0) to nodes(1)
    (0 to 8).foreach { i =>
      nodes(1).receive()
      nodes(0).receive()
    }

    // We simulate a network failure here by not allowing block #2 to get passed to nodes(1)

    // And then we assume fetchDependencies eventually gets called
    nodes(1).casperEff.fetchDependencies
    nodes(0).receive()

    nodes(1).logEff.infos
      .count(_ startsWith "Requested missing block") should be(10)
    nodes(0).logEff.infos.count(
      s => (s startsWith "Received request for block") && (s endsWith "Response sent.")
    ) should be(10)

    nodes.foreach(_.tearDown())
  }

  it should "increment last finalized block as appropriate in round robin" in {
    val stake      = 10L
    val equalBonds = validators.map(_ -> stake).toMap
    val genesisWithEqualBonds =
      buildGenesis(Seq.empty, equalBonds, 1L, Long.MaxValue, Faucet.noopFaucet, 0L)
    val nodes       = HashSetCasperTestNode.network(validatorKeys.take(3), genesisWithEqualBonds)
    val deployDatas = (0 to 7).map(i => ProtoUtil.basicDeployData[Id](i))

    val Created(block1) = nodes(0).casperEff
      .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block1)
    nodes(1).receive()
    nodes(2).receive()

    val Created(block2) = nodes(1).casperEff
      .deploy(deployDatas(1)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block2)
    nodes(0).receive()
    nodes(2).receive()

    val Created(block3) = nodes(2).casperEff
      .deploy(deployDatas(2)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block3)
    nodes(0).receive()
    nodes(1).receive()

    val Created(block4) = nodes(0).casperEff
      .deploy(deployDatas(3)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block4)
    nodes(1).receive()
    nodes(2).receive()

    val Created(block5) = nodes(1).casperEff
      .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block5)
    nodes(0).receive()
    nodes(2).receive()

    nodes(0).casperEff.lastFinalizedBlock should be(genesisWithEqualBonds)

    val Created(block6) = nodes(2).casperEff
      .deploy(deployDatas(5)) *> nodes(2).casperEff.createBlock
    nodes(2).casperEff.addBlock(block6)
    nodes(0).receive()
    nodes(1).receive()

    nodes(0).casperEff.lastFinalizedBlock should be(block1)

    val Created(block7) = nodes(0).casperEff
      .deploy(deployDatas(6)) *> nodes(0).casperEff.createBlock
    nodes(0).casperEff.addBlock(block7)
    nodes(1).receive()
    nodes(2).receive()

    nodes(0).casperEff.lastFinalizedBlock should be(block2)

    val Created(block8) = nodes(1).casperEff
      .deploy(deployDatas(7)) *> nodes(1).casperEff.createBlock
    nodes(1).casperEff.addBlock(block8)
    nodes(0).receive()
    nodes(2).receive()

    nodes(0).casperEff.lastFinalizedBlock should be(block3)

    nodes.foreach(_.tearDown())
  }

  it should "fail when deploying with insufficient phlos" in {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deployData        <- ProtoUtil.basicDeployData[Effect](0).map(_.withPhloLimit(1))
      _                 <- node.casperEff.deploy(deployData)
      createBlockResult <- MultiParentCasper[Effect].createBlock
      Created(block)    = createBlockResult
    } yield assert(block.body.get.deploys.head.errored)
  }

  it should "succeed if given enough phlos for deploy" in {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deployData <- ProtoUtil.basicDeployData[Effect](0).map(_.withPhloLimit(100))
      _          <- node.casperEff.deploy(deployData)

      createBlockResult <- MultiParentCasper[Effect].createBlock
      Created(block)    = createBlockResult
    } yield assert(!block.body.get.deploys.head.errored)
  }

  private def buildBlockWithInvalidJustification(
      nodes: IndexedSeq[HashSetCasperTestNode[Id]],
      deploys: immutable.IndexedSeq[ProcessedDeploy],
      signedInvalidBlock: BlockMessage
  ) = {
    val postState     = RChainState().withBonds(ProtoUtil.bonds(genesis)).withBlockNumber(1)
    val postStateHash = Blake2b256.hash(postState.toByteArray)
    val header = Header()
      .withPostStateHash(ByteString.copyFrom(postStateHash))
      .withParentsHashList(signedInvalidBlock.header.get.parentsHashList)
      .withDeploysHash(ProtoUtil.protoSeqHash(deploys))
    val blockHash = Blake2b256.hash(header.toByteArray)
    val body      = Body().withState(postState).withDeploys(deploys)
    val serializedJustifications =
      Seq(Justification(signedInvalidBlock.sender, signedInvalidBlock.blockHash))
    val serializedBlockHash = ByteString.copyFrom(blockHash)
    val blockThatPointsToInvalidBlock =
      BlockMessage(serializedBlockHash, Some(header), Some(body), serializedJustifications)
    ProtoUtil.signBlock[Id](
      blockThatPointsToInvalidBlock,
      nodes(1).casperEff.blockDag,
      validators(1),
      validatorKeys(1),
      "ed25519",
      "rchain"
    )
  }
}

object HashSetCasperTest {
  def validateBlockStore[R](
      node: HashSetCasperTestNode[Effect]
  )(f: BlockStore[Effect] => Effect[R])(implicit metrics: Metrics[Effect]) = {
    val bs = BlockDagStorageTestFixture.createBlockStorage[Effect](node.blockStoreDir)
    for {
      result <- f(bs)
      _      <- bs.close()
      _      <- Sync[Effect].delay { node.blockStoreDir.recursivelyDelete() }
    } yield result
  }

  def blockTuplespaceContents(
      block: BlockMessage
  )(implicit casper: MultiParentCasper[Effect]): Effect[String] = {
    val tsHash = ProtoUtil.postStateHash(block)
    MultiParentCasper[Effect].storageContents(tsHash)
  }

  def deployAndQuery(
      node: HashSetCasperTestNode[Id],
      dd: DeployData,
      query: Deploy
  ): (BlockStatus, Seq[Par]) = {
    val Created(block) = node.casperEff.deploy(dd) *> node.casperEff.createBlock
    val blockStatus    = node.casperEff.addBlock(block)
    val queryResult = node.runtimeManager
      .captureResults(
        ProtoUtil.postStateHash(block),
        query
      )

    (blockStatus, queryResult)
  }

  def createBonds(validators: Seq[Array[Byte]]): Map[Array[Byte], Long] =
    validators.zipWithIndex.map { case (v, i) => v -> (2L * i.toLong + 1L) }.toMap

  def createGenesis(bonds: Map[Array[Byte], Long]): BlockMessage =
    buildGenesis(Seq.empty, bonds, 1L, Long.MaxValue, Faucet.noopFaucet, 0L)

  def buildGenesis(
      wallets: Seq[PreWallet],
      bonds: Map[Array[Byte], Long],
      minimumBond: Long,
      maximumBond: Long,
      faucetCode: String => String,
      deployTimestamp: Long
  ): BlockMessage = {
    val initial           = Genesis.withoutContracts(bonds, 1L, deployTimestamp, "rchain")
    val storageDirectory  = Files.createTempDirectory(s"hash-set-casper-test-genesis")
    val storageSize: Long = 1024L * 1024
    val activeRuntime     = Runtime.create(storageDirectory, storageSize)
    val runtimeManager    = RuntimeManager.fromRuntime(activeRuntime)
    val emptyStateHash    = runtimeManager.emptyStateHash
    val validators        = bonds.map(bond => ProofOfStakeValidator(bond._1, bond._2)).toSeq
    val genesis = Genesis.withContracts(
      initial,
      ProofOfStakeParams(minimumBond, maximumBond, validators),
      wallets,
      faucetCode,
      emptyStateHash,
      runtimeManager,
      deployTimestamp
    )
    activeRuntime.close().unsafeRunSync
    genesis
  }
}
