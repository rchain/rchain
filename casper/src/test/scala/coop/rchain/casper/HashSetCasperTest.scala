package coop.rchain.casper

import java.nio.file.Files

import cats.{Applicative, Monad}
import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode.Effect
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, BlockUtil, HashSetCasperTestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{BondingUtil, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.{chooseNonConflicting, signBlock, toJustification}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.{transport, CommError, TimeOut}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Keccak256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.{accounting, Runtime}
import coop.rchain.models.{Expr, Par}
import coop.rchain.shared.StoreType
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.eitherT._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Inspectors, Matchers}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.comm.TestNetwork
import coop.rchain.catscontrib.ski.kp2
import coop.rchain.comm.rp.Connect.Connections
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import org.scalatest

import scala.collection.immutable
import scala.util.Random
import scala.concurrent.duration._

class HashSetCasperTest extends FlatSpec with Matchers with Inspectors {

  import HashSetCasperTest._

  implicit val timeEff = new LogicalTime[Effect]

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

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "HashSetCasper" should "accept deploys" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deploy <- ProtoUtil.basicDeployData[Effect](0)
      _      <- MultiParentCasper[Effect].deploy(deploy)

      _      = logEff.infos.size should be(2)
      result = logEff.infos(1).contains("Received Deploy") should be(true)
      _      <- node.tearDown()
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
                 Task
                   .racePair(
                     casper.addBlock(block, ignoreDoppelgangerCheck[Effect]).value,
                     casper.addBlock(block, ignoreDoppelgangerCheck[Effect]).value
                   )
                   .flatMap {
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
    node.tearDown().value.unsafeRunSync
  }

  it should "create blocks based on deploys" in effectTest {
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
      _      = deploys.head should be(deploy)
      result = storage.contains("@{0}!(0)") should be(true)
      _      <- node.tearDown()
    } yield result
  }

  it should "accept signed blocks" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      deploy               <- ProtoUtil.basicDeployData[Effect](0)
      _                    <- MultiParentCasper[Effect].deploy(deploy)
      createBlockResult    <- MultiParentCasper[Effect].createBlock
      Created(signedBlock) = createBlockResult
      _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
      _                    = logEff.warns.isEmpty should be(true)
      dag                  <- MultiParentCasper[Effect].blockDag
      result               <- MultiParentCasper[Effect].estimator(dag) shouldBeF IndexedSeq(signedBlock)
      _                    <- node.tearDown()
    } yield result
  }

  it should "be able to use the registry" in effectTest {
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
      blockStatus       <- casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
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
      block2Status       <- casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _                  = blockStatus shouldBe Valid
      _                  = block2Status shouldBe Valid
      result <- casperEff
                 .storageContents(ProtoUtil.postStateHash(block2))
                 .map(_.contains("Hello, World!")) shouldBeF true

      _ <- node.tearDown()
    } yield result
  }

  it should "be able to create a chain of blocks from different deploys" in effectTest {
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
      _                     <- MultiParentCasper[Effect].addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
      createBlockResult2 <- MultiParentCasper[Effect].deploy(deployDatas(1)) *> MultiParentCasper[
                             Effect
                           ].createBlock
      Created(signedBlock2) = createBlockResult2
      _                     <- MultiParentCasper[Effect].addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
      storage               <- blockTuplespaceContents(signedBlock2)

      _      = logEff.warns should be(Nil)
      _      = ProtoUtil.parentHashes(signedBlock2) should be(Seq(signedBlock1.blockHash))
      dag    <- MultiParentCasper[Effect].blockDag
      _      <- MultiParentCasper[Effect].estimator(dag) shouldBeF IndexedSeq(signedBlock2)
      result = storage.contains("!(12)") should be(true)
      _      <- node.tearDown()
    } yield result
  }

  it should "allow multiple deploys in a single block" in effectTest {
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
      _                 <- MultiParentCasper[Effect].addBlock(block, ignoreDoppelgangerCheck[Effect])
      result            <- MultiParentCasper[Effect].contains(block) shouldBeF true
      _                 <- node.tearDown()
    } yield result
  }

  it should "reject unsigned blocks" in effectTest {
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
      _              <- MultiParentCasper[Effect].addBlock(invalidBlock, ignoreDoppelgangerCheck[Effect])

      _ <- node.casperEff.contains(invalidBlock) shouldBeF false
      _ = logEff.warns.head.contains("Ignoring block") should be(true)
      _ <- node.tearDownNode()
      result <- validateBlockStore(node) { blockStore =>
                 blockStore.get(block.blockHash) shouldBeF None
               }
    } yield result
  }

  it should "not request invalid blocks from peers" in effectTest {

    val List(data0, data1) =
      (0 to 1)
        .map(i => ProtoUtil.sourceDeploy(s"@$i!($i)", i, accounting.MAX_VALUE))
        .toList

    for {
      nodes              <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      List(node0, node1) = nodes.toList

      unsignedBlock <- (node0.casperEff.deploy(data0) *> node0.casperEff.createBlock)
                        .map {
                          case Created(block) =>
                            block.copy(sigAlgorithm = "invalid", sig = ByteString.EMPTY)
                        }

      _ <- node0.casperEff.addBlock(unsignedBlock, ignoreDoppelgangerCheck[Effect])
      _ <- node1.transportLayerEff.clear(node1.local) //node1 misses this block

      signedBlock <- (node0.casperEff.deploy(data1) *> node0.casperEff.createBlock)
                      .map { case Created(block) => block }

      _ <- node0.casperEff.addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
      _ <- node1.receive() //receives block1; should not ask for block0

      _ <- node0.casperEff.contains(unsignedBlock) shouldBeF false
      _ <- node1.casperEff.contains(unsignedBlock) shouldBeF false

    } yield ()
  }

  it should "reject blocks not from bonded validators" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, otherSk)
    import node._
    implicit val timeEff = new LogicalTime[Effect]

    for {
      basicDeployData <- ProtoUtil.basicDeployData[Effect](0)
      createBlockResult <- MultiParentCasper[Effect].deploy(basicDeployData) *> MultiParentCasper[
                            Effect
                          ].createBlock
      Created(signedBlock) = createBlockResult
      _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
      _                    = logEff.warns.head.contains("Ignoring block") should be(true)
      _                    <- node.tearDownNode()
      result <- validateBlockStore(node) { blockStore =>
                 blockStore.get(signedBlock.blockHash) shouldBeF None
               }
    } yield result
  }

  it should "propose blocks it adds to peers" in effectTest {
    for {
      nodes                <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deployData           <- ProtoUtil.basicDeployData[Effect](0)
      createBlockResult    <- nodes(0).casperEff.deploy(deployData) *> nodes(0).casperEff.createBlock
      Created(signedBlock) = createBlockResult
      _                    <- nodes(0).casperEff.addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
      _                    <- nodes(1).receive()
      result               <- nodes(1).casperEff.contains(signedBlock) shouldBeF true
      _                    <- nodes.map(_.tearDownNode()).toList.sequence
      _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
            validateBlockStore(node) { blockStore =>
              blockStore.get(signedBlock.blockHash) shouldBeF Some(signedBlock)
            }(nodes(0).metricEff, nodes(0).logEff)
          }
    } yield result
  }

  it should "add a valid block from peer" in effectTest {
    for {
      nodes                      <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deployData                 <- ProtoUtil.basicDeployData[Effect](1)
      createBlockResult          <- nodes(0).casperEff.deploy(deployData) *> nodes(0).casperEff.createBlock
      Created(signedBlock1Prime) = createBlockResult
      _                          <- nodes(0).casperEff.addBlock(signedBlock1Prime, ignoreDoppelgangerCheck[Effect])
      _                          <- nodes(1).receive()
      _                          = nodes(1).logEff.infos.count(_ startsWith "Added") should be(1)
      result                     = nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(0)
      _                          <- nodes.map(_.tearDownNode()).toList.sequence
      _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
            validateBlockStore(node) { blockStore =>
              blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(signedBlock1Prime)
            }(nodes(0).metricEff, nodes(0).logEff)
          }
    } yield result
  }

  it should "handle multi-parent blocks correctly" in effectTest {
    for {
      nodes       <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deployData0 <- ProtoUtil.basicDeployData[Effect](0)
      deployData2 <- ProtoUtil.basicDeployData[Effect](2)
      deploys = Vector(
        deployData0,
        ProtoUtil.sourceDeploy(
          "@1!(1) | for(@x <- @1){ @1!(x) }",
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        ),
        deployData2
      )
      createBlockResult0 <- nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
      createBlockResult1 <- nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
      Created(block0)    = createBlockResult0
      Created(block1)    = createBlockResult1
      _                  <- nodes(0).casperEff.addBlock(block0, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(1).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()

      //multiparent block joining block0 and block1 since they do not conflict
      multiparentCreateBlockResult <- nodes(0).casperEff
                                       .deploy(deploys(2)) *> nodes(0).casperEff.createBlock
      Created(multiparentBlock) = multiparentCreateBlockResult
      _                         <- nodes(0).casperEff.addBlock(multiparentBlock, ignoreDoppelgangerCheck[Effect])
      _                         <- nodes(1).receive()

      _ = nodes(0).logEff.warns.isEmpty shouldBe true
      _ = nodes(1).logEff.warns.isEmpty shouldBe true
      _ = multiparentBlock.header.get.parentsHashList.size shouldBe 2
      _ = nodes(0).casperEff.contains(multiparentBlock) shouldBeF true
      _ = nodes(1).casperEff.contains(multiparentBlock) shouldBeF true

      finalTuplespace <- nodes(0).casperEff
                          .storageContents(ProtoUtil.postStateHash(multiparentBlock))
      _      = finalTuplespace.contains("@{0}!(0)") shouldBe true
      _      = finalTuplespace.contains("@{1}!(1)") shouldBe true
      result = finalTuplespace.contains("@{2}!(2)") shouldBe true

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "handle multi-parent blocks correctly when they operate on stdout" ignore effectTest {
    def echoContract(no: Int) = s"""new stdout(`rho:io:stdout`) in { stdout!("Contract $no") }"""
    val time                  = System.currentTimeMillis()
    for {
      nodes <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deploys = Vector(
        ProtoUtil.sourceDeploy(echoContract(1), time + 1, accounting.MAX_VALUE),
        ProtoUtil.sourceDeploy(echoContract(2), time + 2, accounting.MAX_VALUE)
      )
      createBlockResult0 <- nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
      createBlockResult1 <- nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
      Created(block0)    = createBlockResult0
      Created(block1)    = createBlockResult1
      _                  <- nodes(0).casperEff.addBlock(block0, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(1).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()

      //multiparent block joining block0 and block1 since they do not conflict
      multiparentCreateBlockResult <- nodes(0).casperEff
                                       .deploy(deploys(1)) *> nodes(0).casperEff.createBlock
      Created(multiparentBlock) = multiparentCreateBlockResult
      _                         <- nodes(0).casperEff.addBlock(multiparentBlock, ignoreDoppelgangerCheck[Effect])
      _                         <- nodes(1).receive()

      _ = nodes(0).logEff.warns.isEmpty shouldBe true
      _ = nodes(1).logEff.warns.isEmpty shouldBe true
      _ = multiparentBlock.header.get.parentsHashList.size shouldBe 2
      _ = nodes(0).casperEff.contains(multiparentBlock) shouldBeF true
      _ = nodes(1).casperEff.contains(multiparentBlock) shouldBeF true
      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield ()
  }

  it should "not merge blocks that touch the same channel" in effectTest {
    for {
      nodes    <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      current0 <- timeEff.currentMillis
      deploy0 = ProtoUtil.sourceDeploy(
        "@1!(47)",
        current0,
        accounting.MAX_VALUE
      )
      current1 <- timeEff.currentMillis
      deploy1 = ProtoUtil.sourceDeploy(
        "for(@x <- @1){ @1!(x) }",
        current1,
        accounting.MAX_VALUE
      )
      deploy2 <- ProtoUtil.basicDeployData[Effect](2)
      deploys = Vector(
        deploy0,
        deploy1,
        deploy2
      )
      createBlock0Result <- nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
      Created(block0)    = createBlock0Result
      createBlock1Result <- nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
      Created(block1)    = createBlock1Result
      _                  <- nodes(0).casperEff.addBlock(block0, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(1).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()

      createSingleParentBlockResult <- nodes(0).casperEff
                                        .deploy(deploys(2)) *> nodes(0).casperEff.createBlock
      Created(singleParentBlock) = createSingleParentBlockResult
      _                          <- nodes(0).casperEff.addBlock(singleParentBlock, ignoreDoppelgangerCheck[Effect])
      _                          <- nodes(1).receive()

      _      = nodes(0).logEff.warns.isEmpty shouldBe true
      _      = nodes(1).logEff.warns.isEmpty shouldBe true
      _      = singleParentBlock.header.get.parentsHashList.size shouldBe 1
      _      <- nodes(0).casperEff.contains(singleParentBlock) shouldBeF true
      result <- nodes(1).casperEff.contains(singleParentBlock) shouldBeF true

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "not produce UnusedCommEvent while merging non conflicting blocks in the presence of conflicting ones" in effectTest {
    def defineDeploy(source: String, t: Long) =
      ProtoUtil.sourceDeploy(
        source,
        t,
        accounting.MAX_VALUE
      )

    val registryRho =
      """
        |// Expected output
        |//
        |// "REGISTRY_SIMPLE_INSERT_TEST: create arbitrary process X to store in the registry"
        |// Unforgeable(0xd3f4cbdcc634e7d6f8edb05689395fef7e190f68fe3a2712e2a9bbe21eb6dd10)
        |// "REGISTRY_SIMPLE_INSERT_TEST: adding X to the registry and getting back a new identifier"
        |// `rho:id:pnrunpy1yntnsi63hm9pmbg8m1h1h9spyn7zrbh1mcf6pcsdunxcci`
        |// "REGISTRY_SIMPLE_INSERT_TEST: got an identifier for X from the registry"
        |// "REGISTRY_SIMPLE_LOOKUP_TEST: looking up X in the registry using identifier"
        |// "REGISTRY_SIMPLE_LOOKUP_TEST: got X from the registry using identifier"
        |// Unforgeable(0xd3f4cbdcc634e7d6f8edb05689395fef7e190f68fe3a2712e2a9bbe21eb6dd10)
        |
        |new simpleInsertTest, simpleInsertTestReturnID, simpleLookupTest,
        |    signedInsertTest, signedInsertTestReturnID, signedLookupTest,
        |    ri(`rho:registry:insertArbitrary`),
        |    rl(`rho:registry:lookup`),
        |    stdout(`rho:io:stdout`),
        |    stdoutAck(`rho:io:stdoutAck`), ack in {
        |        simpleInsertTest!(*simpleInsertTestReturnID) |
        |        for(@idFromTest1 <- simpleInsertTestReturnID) {
        |            simpleLookupTest!(idFromTest1, *ack)
        |        } |
        |
        |        contract simpleInsertTest(registryIdentifier) = {
        |            stdout!("REGISTRY_SIMPLE_INSERT_TEST: create arbitrary process X to store in the registry") |
        |            new X, Y, innerAck in {
        |                stdoutAck!(*X, *innerAck) |
        |                for(_ <- innerAck){
        |                    stdout!("REGISTRY_SIMPLE_INSERT_TEST: adding X to the registry and getting back a new identifier") |
        |                    ri!(*X, *Y) |
        |                    for(@uri <- Y) {
        |                        stdout!("REGISTRY_SIMPLE_INSERT_TEST: got an identifier for X from the registry") |
        |                        stdout!(uri) |
        |                        registryIdentifier!(uri)
        |                    }
        |                }
        |            }
        |        } |
        |
        |        contract simpleLookupTest(@uri, result) = {
        |            stdout!("REGISTRY_SIMPLE_LOOKUP_TEST: looking up X in the registry using identifier") |
        |            new lookupResponse in {
        |                rl!(uri, *lookupResponse) |
        |                for(@val <- lookupResponse) {
        |                    stdout!("REGISTRY_SIMPLE_LOOKUP_TEST: got X from the registry using identifier") |
        |                    stdoutAck!(val, *result)
        |                }
        |            }
        |        }
        |    }
      """.stripMargin

    val tuplesRho =
      """
        |// tuples only support random access
        |new stdout(`rho:io:stdout`) in {
        |
        |  // prints 2 because tuples are 0-indexed
        |  stdout!((1,2,3).nth(1))
        |}
      """.stripMargin
    val timeRho =
      """
        |new timestamp(`rho:block:timestamp`), stdout(`rho:io:stdout`), tCh in {
        |  timestamp!(*tCh) |
        |  for(@t <- tCh) {
        |    match t {
        |      Nil => { stdout!("no block time; no blocks yet? Not connected to Casper network?") }
        |      _ => { stdout!({"block time": t}) }
        |    }
        |  }
        |}
      """.stripMargin

    for {
      nodes  <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis)
      n1     = nodes(0)
      n2     = nodes(1)
      n3     = nodes(2)
      short  = defineDeploy("new x in { x!(0) }", 1L)
      time   = defineDeploy(timeRho, 3L)
      tuples = defineDeploy(tuplesRho, 2L)
      reg    = defineDeploy(registryRho, 4L)

      cB1N3Result   <- n3.casperEff.deploy(short) *> n3.casperEff.createBlock
      Created(b1n3) = cB1N3Result
      _             <- n3.casperEff.addBlock(b1n3, ignoreDoppelgangerCheck[Effect])

      cB1N2Result   <- n2.casperEff.deploy(time) *> n2.casperEff.createBlock
      Created(b1n2) = cB1N2Result
      _             <- n2.casperEff.addBlock(b1n2, ignoreDoppelgangerCheck[Effect])

      cB1N1Result   <- n1.casperEff.deploy(tuples) *> n1.casperEff.createBlock
      Created(b1n1) = cB1N1Result
      _             <- n1.casperEff.addBlock(b1n1, ignoreDoppelgangerCheck[Effect])

      _ <- n2.receive()

      cB2N2Result   <- n2.casperEff.deploy(reg) *> n2.casperEff.createBlock
      Created(b2n2) = cB2N2Result

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield ()
  }

  it should "not merge blocks that touch the same channel involving joins" in effectTest {
    for {
      nodes    <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      current0 <- timeEff.currentMillis
      deploy0 = ProtoUtil.sourceDeploy(
        "@1!(47)",
        current0,
        accounting.MAX_VALUE
      )
      current1 <- timeEff.currentMillis
      deploy1 = ProtoUtil.sourceDeploy(
        "for(@x <- @1; @y <- @2){ @1!(x) }",
        current1,
        accounting.MAX_VALUE
      )
      deploy2 <- ProtoUtil.basicDeployData[Effect](2)
      deploys = Vector(
        deploy0,
        deploy1,
        deploy2
      )

      createBlock0Result <- nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
      Created(block0)    = createBlock0Result
      createBlock1Result <- nodes(1).casperEff.deploy(deploys(1)) *> nodes(1).casperEff.createBlock
      Created(block1)    = createBlock1Result
      _                  <- nodes(0).casperEff.addBlock(block0, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(1).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()
      _                  <- nodes(0).receive()
      _                  <- nodes(1).receive()

      createSingleParentBlockResult <- nodes(0).casperEff
                                        .deploy(deploys(2)) *> nodes(0).casperEff.createBlock
      Created(singleParentBlock) = createSingleParentBlockResult
      _                          <- nodes(0).casperEff.addBlock(singleParentBlock, ignoreDoppelgangerCheck[Effect])
      _                          <- nodes(1).receive()

      _      = nodes(0).logEff.warns.isEmpty shouldBe true
      _      = nodes(1).logEff.warns.isEmpty shouldBe true
      _      = singleParentBlock.header.get.parentsHashList.size shouldBe 1
      _      <- nodes(0).casperEff.contains(singleParentBlock) shouldBeF true
      result <- nodes(1).casperEff.contains(singleParentBlock) shouldBeF true

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "allow bonding" in effectTest {
    for {
      nodes <- HashSetCasperTestNode.networkEff(
                validatorKeys :+ otherSk,
                genesis,
                storageSize = 1024L * 1024 * 10
              )
      runtimeManager = nodes(0).runtimeManager
      pubKey         = Base16.encode(ethPubKeys.head.bytes.drop(1))
      secKey         = ethPivKeys.head.bytes
      ethAddress     = ethAddresses.head
      bondKey        = Base16.encode(otherPk)
      walletUnlockDeploy <- RevIssuanceTest.preWalletUnlockDeploy(
                             ethAddress,
                             pubKey,
                             secKey,
                             "unlockOut"
                           )(Concurrent[Effect], runtimeManager)
      bondingForwarderAddress = BondingUtil.bondingForwarderAddress(ethAddress)
      bondingForwarderDeploy = ProtoUtil.sourceDeploy(
        BondingUtil.bondingForwarderDeploy(bondKey, ethAddress),
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      transferStatusOut = BondingUtil.transferStatusOut(ethAddress)
      bondingTransferDeploy <- RevIssuanceTest.walletTransferDeploy(
                                0,
                                wallets.head.initRevBalance.toLong,
                                bondingForwarderAddress,
                                transferStatusOut,
                                pubKey,
                                secKey
                              )(Concurrent[Effect], runtimeManager)

      createBlock1Result <- nodes(0).casperEff.deploy(walletUnlockDeploy) *> nodes(0).casperEff
                             .deploy(bondingForwarderDeploy) *> nodes(0).casperEff.createBlock
      Created(block1) = createBlock1Result
      block1Status    <- nodes(0).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.toList.traverse_(_.receive()) //send to all peers

      createBlock2Result <- nodes(1).casperEff
                             .deploy(bondingTransferDeploy) *> nodes(1).casperEff.createBlock
      Created(block2) = createBlock2Result
      block2Status    <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.toList.traverse_(_.receive())

      helloWorldDeploy = ProtoUtil.sourceDeploy(
        """new s(`rho:io:stdout`) in { s!("Hello, World!") }""",
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
      //new validator does deploy/propose
      createBlock3Result <- nodes.last.casperEff
                             .deploy(helloWorldDeploy) *> nodes.last.casperEff.createBlock
      Created(block3) = createBlock3Result
      block3Status    <- nodes.last.casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])

      //previous validator does deploy/propose
      createBlock3PrimeResult <- nodes.head.casperEff
                                  .deploy(helloWorldDeploy) *> nodes.head.casperEff.createBlock
      Created(block3Prime) = createBlock3PrimeResult
      block3PrimeStatus <- nodes.head.casperEff
                            .addBlock(block3Prime, ignoreDoppelgangerCheck[Effect])

      _ <- nodes.toList.traverse_(_.receive()) //all nodes get the blocks

      _ = block1Status shouldBe Valid
      _ = block2Status shouldBe Valid
      _ = block3Status shouldBe Valid
      _ = block3PrimeStatus shouldBe Valid
      _ = nodes.forall(_.logEff.warns.isEmpty) shouldBe true

      rankedValidatorQuery = ProtoUtil.sourceDeploy(
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
      validatorBondsAndRanksT <- runtimeManager
                                  .captureResults(
                                    ProtoUtil.postStateHash(block1),
                                    rankedValidatorQuery
                                  )

      validatorBondsAndRanks: Seq[(ByteString, Long, Int)] = validatorBondsAndRanksT.head.exprs.head.getEListBody.ps
        .map(
          _.exprs.head.getETupleBody.ps match {
            case Seq(a, b, c) =>
              (a.exprs.head.getGByteArray, b.exprs.head.getGInt, c.exprs.head.getGInt.toInt)
          }
        )

      correctBonds = validatorBondsAndRanks.map {
        case (keyA, stake, _) =>
          Bond(keyA, stake)
      }.toSet + Bond(
        ByteString.copyFrom(otherPk),
        wallets.head.initRevBalance.toLong
      )

      newBonds = block2.getBody.getState.bonds
      result   = newBonds.toSet shouldBe correctBonds

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "have a working faucet (in testnet)" in effectTest {
    val node = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
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

    for {
      createBlockResult <- casperEff.deploy(createWalletDeploy) *> casperEff.createBlock
      Created(block)    = createBlockResult
      blockStatus       <- casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
      balanceQuery = ProtoUtil.sourceDeploy(
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
      newWalletBalance <- node.runtimeManager
                           .captureResults(
                             ProtoUtil.postStateHash(block),
                             balanceQuery
                           )
      _      = blockStatus shouldBe Valid
      result = newWalletBalance.head.exprs.head.getGInt shouldBe amount

      _ <- node.tearDown()
    } yield result
  }

  it should "allow bonding via the faucet" in effectTest {
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
      block1Status       <- casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      createBlockResult2 <- casperEff.deploy(bondingDeploy) *> casperEff.createBlock
      Created(block2)    = createBlockResult2
      block2Status       <- casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      oldBonds           = block1.getBody.getState.bonds
      newBonds           = block2.getBody.getState.bonds
      _                  = block1Status shouldBe Valid
      _                  = block2Status shouldBe Valid
      result             = (oldBonds.size + 1) shouldBe newBonds.size

      _ <- node.tearDown()
    } yield result
  }

  it should "allow bonding in an existing network" in effectTest {
    def deployment(i: Int): DeployData =
      ProtoUtil.sourceDeploy(s"@$i!({$i})", System.currentTimeMillis() + i, accounting.MAX_VALUE)

    def deploy(
        node: HashSetCasperTestNode[Effect],
        dd: DeployData
    ): Effect[(BlockMessage, Either[Throwable, BlockStatus])] =
      for {
        createBlockResult1    <- node.casperEff.deploy(dd) *> node.casperEff.createBlock
        Created(signedBlock1) = createBlockResult1

        status <- Sync[Effect].attempt(
                   node.casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
                 )
      } yield (signedBlock1, status)

    def stepSplit(
        nodes: List[HashSetCasperTestNode[Effect]]
    ): Effect[List[Either[Throwable, Unit]]] =
      for {
        _  <- nodes.zipWithIndex.traverse { case (n, i) => deploy(n, deployment(i)) }
        vs <- nodes.traverse(v => Sync[Effect].attempt(v.receive()))
      } yield vs

    def propagate(nodes: List[HashSetCasperTestNode[Effect]]): Effect[Unit] =
      for {
        _ <- nodes.traverse(_.receive())
      } yield ()

    def bond(
        node: HashSetCasperTestNode[Effect],
        keys: (Array[Byte], Array[Byte])
    ): Effect[Unit] = {
      implicit val runtimeManager = node.runtimeManager
      val (sk, pk)                = keys
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
        fr       <- deploy(node, forwardDeploy)
        br       <- deploy(node, bondingDeploy)
        oldBonds = fr._1.getBody.getState.bonds
        newBonds = br._1.getBody.getState.bonds
        _        = fr._2 shouldBe Right(Valid)
        _        = br._2 shouldBe Right(Valid)
        _        = (oldBonds.size + 1) shouldBe newBonds.size
      } yield ()
    }

    val network = TestNetwork.empty[Effect]

    for {
      nodes <- HashSetCasperTestNode
                .networkEff(validatorKeys.take(3), genesis, testNetwork = network)
                .map(_.toList)
      _        <- stepSplit(nodes)
      _        <- stepSplit(nodes)
      (sk, pk) = Ed25519.newKeyPair
      newNode  = HashSetCasperTestNode.standaloneEff(genesis, sk, testNetwork = network)
      _        <- bond(nodes(0), (sk, pk))
      all      <- HashSetCasperTestNode.rigConnectionsF[Effect](newNode, nodes)

      s1 <- stepSplit(all)
      _ = forAll(s1) { v =>
        v.isRight should be(true)
      }
      s2 <- stepSplit(all)
      _ = forAll(s2) { v =>
        v.isRight should be(true)
      }

      _ <- all.map(_.tearDown()).sequence
    } yield ()
  }

  it should "estimate parent properly" in effectTest {
    val (otherSk, otherPk)          = Ed25519.newKeyPair
    val (validatorKeys, validators) = (1 to 5).map(_ => Ed25519.newKeyPair).unzip
    val (ethPivKeys, ethPubKeys)    = (1 to 5).map(_ => Secp256k1.newKeyPair).unzip
    val ethAddresses =
      ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
    val wallets = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
    val bonds = Map(
      validators(0) -> 3L,
      validators(1) -> 1L,
      validators(2) -> 5L,
      validators(3) -> 2L,
      validators(4) -> 4L
    )
    val minimumBond = 100L
    val genesis =
      buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)

    def deployment(i: Int, ts: Long): DeployData =
      ProtoUtil.sourceDeploy(s"new x in { x!(0) }", ts, accounting.MAX_VALUE)

    def deploy(
        node: HashSetCasperTestNode[Effect],
        dd: DeployData
    ) = node.casperEff.deploy(dd)

    def create(
        node: HashSetCasperTestNode[Effect]
    ) =
      for {
        createBlockResult1    <- node.casperEff.createBlock
        Created(signedBlock1) = createBlockResult1
      } yield signedBlock1

    def add(node: HashSetCasperTestNode[Effect], signed: BlockMessage) =
      Sync[Effect].attempt(
        node.casperEff.addBlock(signed, ignoreDoppelgangerCheck[Effect])
      )

    val network = TestNetwork.empty[Effect]

    for {
      nodes <- HashSetCasperTestNode
                .networkEff(validatorKeys.take(3), genesis, testNetwork = network)
                .map(_.toList)
      v1   = nodes(0)
      v2   = nodes(1)
      v3   = nodes(2)
      _    <- deploy(v1, deployment(0, 1)) >> create(v1) >>= (v1c1 => add(v1, v1c1)) //V1#1
      v2c1 <- deploy(v2, deployment(0, 2)) >> create(v2) //V2#1
      _    <- v2.receive()
      _    <- v3.receive()
      _    <- deploy(v1, deployment(0, 4)) >> create(v1) >>= (v1c2 => add(v1, v1c2)) //V1#2
      v3c2 <- deploy(v3, deployment(0, 5)) >> create(v3) //V3#2
      _    <- v3.receive()
      _    <- add(v3, v3c2) //V3#2
      _    <- add(v2, v2c1) //V2#1
      _    <- v3.receive()
      r    <- deploy(v3, deployment(0, 6)) >> create(v3) >>= (b => add(v3, b))
      _    = r shouldBe Right(Valid)
      _    = v3.logEff.warns shouldBe empty

      _ <- nodes.map(_.tearDown()).sequence
    } yield ()
  }

  it should "not fail if the forkchoice changes after a bonding event" in effectTest {
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
      bondingCode <- BondingUtil
                      .faucetBondDeploy[Effect](50, "ed25519", pkStr, sk)(
                        Concurrent[Effect],
                        rm
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

      bondedBlockStatus <- nodes.head.casperEff
                            .addBlock(bondedBlock, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(1).receive()
      _ <- nodes.head.receive()
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses bonding

      createBlockResult2 <- {
        val n = nodes(1)
        import n.casperEff._
        (ProtoUtil.basicDeployData[Effect](0) >>= deploy) *> createBlock
      }
      Created(block2) = createBlockResult2
      status2         <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.head.receive()
      _               <- nodes(1).receive()
      _               <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses block built on bonding

      createBlockResult3 <- { //nodes(2) proposes a block
        val n = nodes(2)
        import n.casperEff._
        (ProtoUtil.basicDeployData[Effect](1) >>= deploy) *> createBlock
      }
      Created(block3) = createBlockResult3
      status3         <- nodes(2).casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])
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
      status4         <- nodes.head.casperEff.addBlock(block4, ignoreDoppelgangerCheck[Effect])
      _               <- nodes.toList.traverse_(_.receive())

      _      = bondedBlockStatus shouldBe Valid
      _      = status2 shouldBe Valid
      _      = status3 shouldBe Valid
      result = status4 shouldBe Valid
      _      = nodes.foreach(_.logEff.warns shouldBe Nil)

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "allow paying for deploys" in effectTest {
    val node      = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
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
    for {
      capturedResults <- node.runtimeManager
                          .captureResults(
                            ProtoUtil.postStateHash(genesis),
                            sigDeployData
                          )
      sigData     = capturedResults.head.exprs.head.getGByteArray
      sig         = Base16.encode(Ed25519.sign(sigData.toByteArray, sk))
      pkStr       = Base16.encode(pk)
      paymentCode = s"""new
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
      paymentDeployData = ProtoUtil
        .sourceDeploy(paymentCode, timestamp, accounting.MAX_VALUE)
        .withPhloPrice(phloPrice)
        .withUser(user)

      paymentQuery = ProtoUtil.sourceDeploy(
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

      deployQueryResult <- deployAndQuery(
                            node,
                            paymentDeployData,
                            paymentQuery
                          )
      (blockStatus, queryResult) = deployQueryResult
      (codeHashPar, _, userIdPar, timestampPar) = ProtoUtil.getRholangDeployParams(
        paymentDeployData
      )
      phloPurchasedPar = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(phloPrice * amount))))
      _                = blockStatus shouldBe Valid
      result = queryResult.head.exprs.head.getETupleBody.ps shouldBe Seq(
        codeHashPar,
        userIdPar,
        timestampPar,
        phloPurchasedPar
      )
      _ <- node.tearDown()
    } yield result
  }

  it should "reject addBlock when there exist deploy by the same (user, millisecond timestamp) in the chain" in effectTest {
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
      _                     <- nodes(0).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
      _                     <- nodes(1).receive() // receive block1

      createBlockResult2 <- nodes(0).casperEff
                             .deploy(deployDatas(1)) *> nodes(0).casperEff.createBlock
      Created(signedBlock2) = createBlockResult2
      _                     <- nodes(0).casperEff.addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
      _                     <- nodes(1).receive() // receive block2

      createBlockResult3 <- nodes(0).casperEff
                             .deploy(deployDatas(2)) *> nodes(0).casperEff.createBlock
      Created(signedBlock3) = createBlockResult3
      _                     <- nodes(0).casperEff.addBlock(signedBlock3, ignoreDoppelgangerCheck[Effect])
      _                     <- nodes(1).receive() // receive block3

      _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true

      createBlockResult4 <- nodes(1).casperEff
                             .deploy(deployPrim0) *> nodes(1).casperEff.createBlock
      Created(signedBlock4) = createBlockResult4
      _ <- nodes(1).casperEff
            .addBlock(signedBlock4, ignoreDoppelgangerCheck[Effect]) // should succeed
      _ <- nodes(0).receive() // still receive signedBlock4

      result <- nodes(1).casperEff
                 .contains(signedBlock4) shouldBeF true // Invalid blocks are still added
      // TODO: Fix with https://rchain.atlassian.net/browse/RHOL-1048
      // nodes(0).casperEff.contains(signedBlock4) should be(false)
      //
      // nodes(0).logEff.warns
      //   .count(_ contains "found deploy by the same (user, millisecond timestamp) produced") should be(
      //   1
      // )
      _ <- nodes.map(_.tearDownNode()).toList.sequence

      _ = nodes.toList.traverse_[Effect, Assertion] { node =>
        validateBlockStore(node) { blockStore =>
          for {
            _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
            _      <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
            result <- blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
          } yield result
        }(nodes(0).metricEff, nodes(0).logEff)
      }
    } yield result
  }

  it should "ask peers for blocks it is missing" in effectTest {
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

      _ <- nodes(0).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(1).receive()
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block

      createBlockResult2 <- nodes(0).casperEff
                             .deploy(deployDatas(1)) *> nodes(0).casperEff.createBlock
      Created(signedBlock2) = createBlockResult2

      _ <- nodes(0).casperEff.addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
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

      _ <- nodes.map(_.tearDownNode()).toList.sequence
      _ <- nodes.toList.traverse_[Effect, Assertion] { node =>
            validateBlockStore(node) { blockStore =>
              for {
                _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
                result <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
              } yield result
            }(nodes(0).metricEff, nodes(0).logEff)
          }
    } yield result
  }

  /*
   *  DAG Looks like this:
   *
   *             h1
   *            /  \
   *           g1   g2
   *           |  X |
   *           f1   f2
   *            \  /
   *             e1
   *             |
   *             d1
   *            /  \
   *           c1   c2
   *           |  X |
   *           b1   b2
   *           |  X |
   *           a1   a2
   *            \  /
   *          genesis
   *
   * f2 has in its justifications list c2. This should be handled properly.
   *
   */
  it should "ask peers for blocks it is missing and add them" in effectTest {
    val deployDatasFs = Vector(
      "@2!(2)",
      "@1!(1)"
    ).zipWithIndex
      .map(
        d =>
          () =>
            ProtoUtil.sourceDeploy(d._1, System.currentTimeMillis() + d._2, accounting.MAX_VALUE)
      )
    def deploy(node: HashSetCasperTestNode[Effect], dd: DeployData): Effect[BlockMessage] =
      for {
        createBlockResult1    <- node.casperEff.deploy(dd) *> node.casperEff.createBlock
        Created(signedBlock1) = createBlockResult1

        _ <- node.casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
      } yield signedBlock1

    def stepSplit(nodes: Seq[HashSetCasperTestNode[Effect]]) =
      for {
        _ <- deploy(nodes(0), deployDatasFs(0).apply())
        _ <- deploy(nodes(1), deployDatasFs(1).apply())

        _ <- nodes(0).receive()
        _ <- nodes(1).receive()
        _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block
      } yield ()

    def stepSingle(nodes: Seq[HashSetCasperTestNode[Effect]]) =
      for {
        _ <- deploy(nodes(0), deployDatasFs(0).apply())

        _ <- nodes(0).receive()
        _ <- nodes(1).receive()
        _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block
      } yield ()

    def propagate(nodes: Seq[HashSetCasperTestNode[Effect]]) =
      for {
        _ <- nodes(0).receive()
        _ <- nodes(1).receive()
        _ <- nodes(2).receive()
      } yield ()

    for {
      nodes <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis)

      _ <- stepSplit(nodes) // blocks a1 a2
      _ <- stepSplit(nodes) // blocks b1 b2
      _ <- stepSplit(nodes) // blocks c1 c2

      _ <- stepSingle(nodes) // block d1
      _ <- stepSingle(nodes) // block e1

      _ <- stepSplit(nodes) // blocks f1 f2
      _ <- stepSplit(nodes) // blocks g1 g2

      // this block will be propagated to all nodes and force nodes(2) to ask for missing blocks.
      br <- deploy(nodes(0), deployDatasFs(0).apply()) // block h1

      _ <- List.fill(22)(propagate(nodes)).toList.sequence // force the network to communicate

      _ <- nodes(2).casperEff.contains(br) shouldBeF true

      nr <- deploy(nodes(2), deployDatasFs(0).apply())
      _  = nr.header.get.parentsHashList shouldBe Seq(br.blockHash)
      _  <- nodes.map(_.tearDownNode()).toList.sequence
    } yield ()
  }

  it should "ignore adding equivocation blocks" in effectTest {
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

      _ <- nodes(0).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(1).receive()
      _ <- nodes(0).casperEff.addBlock(signedBlock1Prime, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(1).receive()

      _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
      result <- nodes(1).casperEff
                 .contains(signedBlock1Prime) shouldBeF false // we still add the equivocation pair

      _ <- nodes(0).tearDownNode()
      _ <- nodes(1).tearDownNode()
      _ <- validateBlockStore(nodes(1)) { blockStore =>
            for {
              _      <- blockStore.get(signedBlock1.blockHash) shouldBeF Some(signedBlock1)
              result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF None
            } yield result
          }(nodes(0).metricEff, nodes(0).logEff)
    } yield result
  }

  // See [[/docs/casper/images/minimal_equivocation_neglect.png]] but cross out genesis block
  it should "not ignore equivocation blocks that are required for parents of proper nodes" in effectTest {
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

      _ <- nodes(1).casperEff.addBlock(signedBlock1, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(0).transportLayerEff.clear(nodes(0).local) //nodes(0) misses this block
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) misses this block

      _ <- nodes(0).casperEff.addBlock(signedBlock1Prime, ignoreDoppelgangerCheck[Effect])
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

      _ <- nodes(2).casperEff.addBlock(signedBlock3, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(1).casperEff.addBlock(signedBlock2, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(2).transportLayerEff.clear(nodes(2).local) //nodes(2) ignores block2
      _ <- nodes(1).receive() // receives block3; asks for block1'
      _ <- nodes(2).receive() // receives request for block1'; sends block1'
      _ <- nodes(1).receive() // receives block1'; adds both block3 and block1'

      _ <- nodes(1).casperEff.contains(signedBlock3) shouldBeF true
      _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

      createBlockResult4 <- nodes(1).casperEff
                             .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
      Created(signedBlock4) = createBlockResult4
      _                     <- nodes(1).casperEff.addBlock(signedBlock4, ignoreDoppelgangerCheck[Effect])

      // Node 1 should contain both blocks constituting the equivocation
      _ <- nodes(1).casperEff.contains(signedBlock1) shouldBeF true
      _ <- nodes(1).casperEff.contains(signedBlock1Prime) shouldBeF true

      _ <- nodes(1).casperEff
            .contains(signedBlock4) shouldBeF true // However, in invalidBlockTracker

      _ = nodes(1).logEff.infos.count(_ startsWith "Added admissible equivocation") should be(1)
      _ = nodes(2).logEff.warns.size should be(0)
      _ = nodes(1).logEff.warns.size should be(1)
      _ = nodes(0).logEff.warns.size should be(0)

      _ <- nodes(1).casperEff
            .normalizedInitialFault(ProtoUtil.weightMap(genesis)) shouldBeF 1f / (1f + 3f + 5f + 7f)
      _ <- nodes.map(_.tearDownNode()).toList.sequence

      _ <- validateBlockStore(nodes(0)) { blockStore =>
            for {
              _ <- blockStore.get(signedBlock1.blockHash) shouldBeF None
              result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
                         signedBlock1Prime
                       )
            } yield result
          }(nodes(0).metricEff, nodes(0).logEff)
      _ <- validateBlockStore(nodes(1)) { blockStore =>
            for {
              _      <- blockStore.get(signedBlock2.blockHash) shouldBeF Some(signedBlock2)
              result <- blockStore.get(signedBlock4.blockHash) shouldBeF Some(signedBlock4)
            } yield result
          }(nodes(1).metricEff, nodes(1).logEff)
      result <- validateBlockStore(nodes(2)) { blockStore =>
                 for {
                   _ <- blockStore.get(signedBlock3.blockHash) shouldBeF Some(signedBlock3)
                   result <- blockStore.get(signedBlock1Prime.blockHash) shouldBeF Some(
                              signedBlock1Prime
                            )
                 } yield result
               }(nodes(2).metricEff, nodes(2).logEff)
    } yield result
  }

  it should "prepare to slash an block that includes a invalid block pointer" in effectTest {
    for {
      nodes           <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis)
      deploys         <- (0 to 5).toList.traverse(i => ProtoUtil.basicDeployData[Effect](i))
      deploysWithCost = deploys.map(d => ProcessedDeploy(deploy = Some(d))).toIndexedSeq

      createBlockResult <- nodes(0).casperEff
                            .deploy(deploys(0)) *> nodes(0).casperEff.createBlock
      Created(signedBlock) = createBlockResult
      signedInvalidBlock = BlockUtil.resignBlock(
        signedBlock.withSeqNum(-2),
        nodes(0).validatorId.privateKey
      ) // Invalid seq num

      blockWithInvalidJustification <- buildBlockWithInvalidJustification(
                                        nodes,
                                        deploysWithCost,
                                        signedInvalidBlock
                                      )

      _ <- nodes(1).casperEff
            .addBlock(blockWithInvalidJustification, ignoreDoppelgangerCheck[Effect])
      _ <- nodes(0).transportLayerEff
            .clear(nodes(0).local) // nodes(0) rejects normal adding process for blockThatPointsToInvalidBlock

      signedInvalidBlockPacketMessage = packet(
        nodes(0).local,
        transport.BlockMessage,
        signedInvalidBlock.toByteString
      )
      _ <- nodes(0).transportLayerEff.send(nodes(1).local, signedInvalidBlockPacketMessage)
      _ <- nodes(1).receive() // receives signedInvalidBlock; attempts to add both blocks

      result = nodes(1).logEff.warns.count(_ startsWith "Recording invalid block") should be(1)
      _      <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "handle a long chain of block requests appropriately" in effectTest {
    for {
      nodes <- HashSetCasperTestNode.networkEff(
                validatorKeys.take(2),
                genesis,
                storageSize = 1024L * 1024 * 10
              )

      _ <- (0 to 9).toList.traverse_[Effect, Unit] { i =>
            for {
              deploy <- ProtoUtil.basicDeployData[Effect](i)
              createBlockResult <- nodes(0).casperEff
                                    .deploy(deploy) *> nodes(0).casperEff.createBlock
              Created(block) = createBlockResult

              _ <- nodes(0).casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
              _ <- nodes(1).transportLayerEff.clear(nodes(1).local) //nodes(1) misses this block
            } yield ()
          }
      deployData10 <- ProtoUtil.basicDeployData[Effect](10)
      createBlock11Result <- nodes(0).casperEff.deploy(deployData10) *> nodes(
                              0
                            ).casperEff.createBlock
      Created(block11) = createBlock11Result
      _                <- nodes(0).casperEff.addBlock(block11, ignoreDoppelgangerCheck[Effect])

      // Cycle of requesting and passing blocks until block #3 from nodes(0) to nodes(1)
      _ <- (0 to 8).toList.traverse_[Effect, Unit] { i =>
            nodes(1).receive() *> nodes(0).receive()
          }

      // We simulate a network failure here by not allowing block #2 to get passed to nodes(1)

      // And then we assume fetchDependencies eventually gets called
      _ <- nodes(1).casperEff.fetchDependencies
      _ <- nodes(0).receive()

      _ = nodes(1).logEff.infos.count(_ startsWith "Requested missing block") should be(10)
      result = nodes(0).logEff.infos.count(
        s => (s startsWith "Received request for block") && (s endsWith "Response sent.")
      ) should be(10)

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield result
  }

  it should "increment last finalized block as appropriate in round robin" in effectTest {
    val stake      = 10L
    val equalBonds = validators.map(_ -> stake).toMap
    val genesisWithEqualBonds =
      buildGenesis(Seq.empty, equalBonds, 1L, Long.MaxValue, Faucet.noopFaucet, 0L)
    for {
      nodes       <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesisWithEqualBonds)
      deployDatas <- (0 to 7).toList.traverse(i => ProtoUtil.basicDeployData[Effect](i))

      createBlock1Result <- nodes(0).casperEff
                             .deploy(deployDatas(0)) *> nodes(0).casperEff.createBlock
      Created(block1) = createBlock1Result
      _               <- nodes(0).casperEff.addBlock(block1, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(1).receive()
      _               <- nodes(2).receive()

      createBlock2Result <- nodes(1).casperEff
                             .deploy(deployDatas(1)) *> nodes(1).casperEff.createBlock
      Created(block2) = createBlock2Result
      _               <- nodes(1).casperEff.addBlock(block2, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(2).receive()

      createBlock3Result <- nodes(2).casperEff
                             .deploy(deployDatas(2)) *> nodes(2).casperEff.createBlock
      Created(block3) = createBlock3Result
      _               <- nodes(2).casperEff.addBlock(block3, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(1).receive()

      createBlock4Result <- nodes(0).casperEff
                             .deploy(deployDatas(3)) *> nodes(0).casperEff.createBlock
      Created(block4) = createBlock4Result
      _               <- nodes(0).casperEff.addBlock(block4, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(1).receive()
      _               <- nodes(2).receive()

      createBlock5Result <- nodes(1).casperEff
                             .deploy(deployDatas(4)) *> nodes(1).casperEff.createBlock
      Created(block5) = createBlock5Result
      _               <- nodes(1).casperEff.addBlock(block5, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(2).receive()

      _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block1
      state <- nodes(0).casperState.read
      _     = state.deployHistory.size should be(1)

      createBlock6Result <- nodes(2).casperEff
                             .deploy(deployDatas(5)) *> nodes(2).casperEff.createBlock
      Created(block6) = createBlock6Result
      _               <- nodes(2).casperEff.addBlock(block6, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(1).receive()

      _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block2
      state <- nodes(0).casperState.read
      _     = state.deployHistory.size should be(1)

      createBlock7Result <- nodes(0).casperEff
                             .deploy(deployDatas(6)) *> nodes(0).casperEff.createBlock
      Created(block7) = createBlock7Result
      _               <- nodes(0).casperEff.addBlock(block7, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(1).receive()
      _               <- nodes(2).receive()

      _ <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block3
      _ = state.deployHistory.size should be(1)

      createBlock8Result <- nodes(1).casperEff
                             .deploy(deployDatas(7)) *> nodes(1).casperEff.createBlock
      Created(block8) = createBlock8Result
      _               <- nodes(1).casperEff.addBlock(block8, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(2).receive()

      _     <- nodes(0).casperEff.lastFinalizedBlock shouldBeF block4
      state <- nodes(0).casperState.read
      _     = state.deployHistory.size should be(1)

      _ <- nodes.map(_.tearDown()).toList.sequence
    } yield ()
  }

  it should "fail when deploying with insufficient phlos" in effectTest {
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

  it should "succeed if given enough phlos for deploy" in effectTest {
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
      nodes: IndexedSeq[HashSetCasperTestNode[Effect]],
      deploys: immutable.IndexedSeq[ProcessedDeploy],
      signedInvalidBlock: BlockMessage
  ): Effect[BlockMessage] = {
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
    nodes(1).casperEff.blockDag.flatMap { dag =>
      ProtoUtil.signBlock[Effect](
        blockThatPointsToInvalidBlock,
        dag,
        validators(1),
        validatorKeys(1),
        "ed25519",
        "rchain"
      )
    }
  }
}

object HashSetCasperTest {
  def validateBlockStore[R](
      node: HashSetCasperTestNode[Effect]
  )(f: BlockStore[Effect] => Effect[R])(implicit metrics: Metrics[Effect], log: Log[Effect]) =
    for {
      bs     <- BlockDagStorageTestFixture.createBlockStorage[Effect](node.blockStoreDir)
      result <- f(bs)
      _      <- bs.close()
      _      <- Sync[Effect].delay { node.blockStoreDir.recursivelyDelete() }
    } yield result

  def blockTuplespaceContents(
      block: BlockMessage
  )(implicit casper: MultiParentCasper[Effect]): Effect[String] = {
    val tsHash = ProtoUtil.postStateHash(block)
    MultiParentCasper[Effect].storageContents(tsHash)
  }

  def deployAndQuery(
      node: HashSetCasperTestNode[Effect],
      dd: DeployData,
      query: DeployData
  ): Effect[(BlockStatus, Seq[Par])] =
    for {
      createBlockResult <- node.casperEff.deploy(dd) *> node.casperEff.createBlock
      Created(block)    = createBlockResult
      blockStatus       <- node.casperEff.addBlock(block, ignoreDoppelgangerCheck[Effect])
      queryResult <- node.runtimeManager
                      .captureResults(ProtoUtil.postStateHash(block), query)
    } yield (blockStatus, queryResult)

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
    val initial                            = Genesis.withoutContracts(bonds, 1L, deployTimestamp, "rchain")
    val storageDirectory                   = Files.createTempDirectory(s"hash-set-casper-test-genesis")
    val storageSize: Long                  = 1024L * 1024
    implicit val log                       = new Log.NOPLog[Task]
    implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]

    val activeRuntime =
      Runtime.create[Task, Task.Par](storageDirectory, storageSize, StoreType.LMDB).unsafeRunSync
    val runtimeManager = RuntimeManager.fromRuntime[Task](activeRuntime).unsafeRunSync
    val emptyStateHash = runtimeManager.emptyStateHash
    val validators     = bonds.map(bond => ProofOfStakeValidator(bond._1, bond._2)).toSeq
    val genesis = Genesis
      .withContracts(
        initial,
        ProofOfStakeParams(minimumBond, maximumBond, validators),
        wallets,
        faucetCode,
        emptyStateHash,
        runtimeManager,
        deployTimestamp
      )
      .unsafeRunSync
    activeRuntime.close().unsafeRunSync
    genesis
  }
}
