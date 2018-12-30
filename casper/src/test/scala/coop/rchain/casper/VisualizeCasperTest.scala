package coop.rchain.casper

import cats.mtl._
import cats.mtl.implicits._
import coop.rchain.graphz._
import java.nio.file.Files
import cats.Applicative
import cats.data.EitherT
import cats.effect.Sync
import cats._, cats.data._, cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.api.BlockAPI
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
import coop.rchain.catscontrib.ski.kp2
import coop.rchain.metrics.Metrics
import coop.rchain.casper.api.GraphzGenerator
import scala.collection.immutable
import scala.util.Random

class VisualizeCasperTest extends FlatSpec with Matchers {

  type GraphEffect[A] = StateT[Id, StringBuffer, A]
  implicit val ser: StringSerializer[GraphEffect]           = new StringSerializer[GraphEffect]
  val stringify: GraphEffect[Graphz[GraphEffect]] => String = _.runS(new StringBuffer).toString

  import HashSetCasperTest._

  implicit val timeEff = new LogicalTime[Effect]

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val (_, ethPubKeys)             = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  private val ethAddresses =
    ethPubKeys.map(pk => "0x" + Base16.encode(Keccak256.hash(pk.bytes.drop(1)).takeRight(20)))
  private val wallets     = ethAddresses.map(addr => PreWallet(addr, BigInt(10001)))
  private val bonds       = createBonds(validators)
  private val minimumBond = 100L
  private val genesis =
    buildGenesis(wallets, bonds, minimumBond, Long.MaxValue, Faucet.basicWalletFaucet, 0L)

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "BlockAPI.visualizeBlocks" should "example 1 - dag with one merge" in effectTest {
    // given
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
      _         = finalTuplespace.contains("@{0}!(0)") shouldBe true
      _         = finalTuplespace.contains("@{1}!(1)") shouldBe true
      result    = finalTuplespace.contains("@{2}!(2)") shouldBe true
      casperRef <- MultiParentCasperRef.of[Effect]
      _         <- casperRef.set(nodes(0).casperEff)
      vis <- {
        implicit val cref          = casperRef
        implicit val logEff        = nodes(0).logEff
        implicit val turanEff      = nodes(0).turanOracleEffect
        implicit val blockStoreEff = nodes(0).blockStore

        BlockAPI.visualizeDag[Effect, GraphEffect](
          None,
          (ts, lfb) => GraphzGenerator.dagAsCluster[Effect, GraphEffect](ts, lfb),
          stringify
        )
      }
      _ = println(vis)
      _ = nodes.foreach(_.tearDown())
    } yield result
  }

  it should "example 2 - 'ideal' blockchain" in effectTest {
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

      _ = nodes(0).casperEff.lastFinalizedBlock shouldBeF genesisWithEqualBonds

      createBlock6Result <- nodes(2).casperEff
                             .deploy(deployDatas(5)) *> nodes(2).casperEff.createBlock
      Created(block6) = createBlock6Result
      _               <- nodes(2).casperEff.addBlock(block6, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(0).receive()
      _               <- nodes(1).receive()

      _ = nodes(0).casperEff.lastFinalizedBlock shouldBeF block1

      createBlock7Result <- nodes(0).casperEff
                             .deploy(deployDatas(6)) *> nodes(0).casperEff.createBlock
      Created(block7) = createBlock7Result
      _               <- nodes(0).casperEff.addBlock(block7, ignoreDoppelgangerCheck[Effect])
      _               <- nodes(1).receive()
      _               <- nodes(2).receive()

      _ = nodes(0).casperEff.lastFinalizedBlock shouldBeF block2

      createBlock8Result <- nodes(1).casperEff
                             .deploy(deployDatas(7)) *> nodes(1).casperEff.createBlock
      Created(block8) = createBlock8Result
      _               = nodes(1).casperEff.addBlock(block8, ignoreDoppelgangerCheck[Effect])
      _               = nodes(0).receive()
      _               = nodes(2).receive()

      result    = nodes(0).casperEff.lastFinalizedBlock shouldBeF block3
      casperRef <- MultiParentCasperRef.of[Effect]
      _         <- casperRef.set(nodes(0).casperEff)
      vis <- {
        implicit val cref          = casperRef
        implicit val logEff        = nodes(0).logEff
        implicit val turanEff      = nodes(0).turanOracleEffect
        implicit val blockStoreEff = nodes(0).blockStore
        BlockAPI.visualizeDag[Effect, GraphEffect](
          None,
          (ts, lfb) => GraphzGenerator.dagAsCluster[Effect, GraphEffect](ts, lfb),
          stringify
        )
      }
      _ = println(vis)
      _ = nodes.foreach(_.tearDown())
    } yield result

  }

  def h(b: BlockMessage): String = PrettyPrinter.buildString(b.blockHash)
}
