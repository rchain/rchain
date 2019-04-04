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
import coop.rchain.casper.util.ProtoUtil.{signBlock, toJustification}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.{transport, CommError, TimeOut}
import coop.rchain.crypto.{PrivateKey, PublicKey}
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

class MultiParentCasperMergeSpec extends FlatSpec with Matchers with Inspectors {

  import MultiParentCasperTestUtil._

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
  "HashSetCasper" should "handle multi-parent blocks correctly" in effectTest {
    for {
      nodes       <- HashSetCasperTestNode.networkEff(validatorKeys.take(2), genesis)
      deployData0 <- ConstructDeploy.basicDeployData[Effect](0)
      deployData2 <- ConstructDeploy.basicDeployData[Effect](2)
      deploys = Vector(
        deployData0,
        ConstructDeploy.sourceDeploy(
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
        ConstructDeploy.sourceDeploy(echoContract(1), time + 1, accounting.MAX_VALUE),
        ConstructDeploy.sourceDeploy(echoContract(2), time + 2, accounting.MAX_VALUE)
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
      deploy0 = ConstructDeploy.sourceDeploy(
        "@1!(47)",
        current0,
        accounting.MAX_VALUE
      )
      current1 <- timeEff.currentMillis
      deploy1 = ConstructDeploy.sourceDeploy(
        "for(@x <- @1){ @1!(x) }",
        current1,
        accounting.MAX_VALUE
      )
      deploy2 <- ConstructDeploy.basicDeployData[Effect](2)
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
      ConstructDeploy.sourceDeploy(
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
      deploy0 = ConstructDeploy.sourceDeploy(
        "@1!(47)",
        current0,
        accounting.MAX_VALUE
      )
      current1 <- timeEff.currentMillis
      deploy1 = ConstructDeploy.sourceDeploy(
        "for(@x <- @1; @y <- @2){ @1!(x) }",
        current1,
        accounting.MAX_VALUE
      )
      deploy2 <- ConstructDeploy.basicDeployData[Effect](2)
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
}
