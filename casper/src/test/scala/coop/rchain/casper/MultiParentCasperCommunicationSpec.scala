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

class MultiParentCasperCommunicationSpec extends FlatSpec with Matchers with Inspectors {

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
  "MultiParentCasper" should "ask peers for blocks it is missing" in effectTest {
    for {
      nodes <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesis)
      deployDatas = Vector(
        "for(_ <- @1){ Nil } | @1!(1)",
        "@2!(2)"
      ).zipWithIndex
        .map(
          d =>
            ConstructDeploy
              .sourceDeploy(d._1, System.currentTimeMillis() + d._2, accounting.MAX_VALUE)
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
            ConstructDeploy
              .sourceDeploy(d._1, System.currentTimeMillis() + d._2, accounting.MAX_VALUE)
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
}
