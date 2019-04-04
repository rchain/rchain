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

class MultiParentCasperFinalizationSpec extends FlatSpec with Matchers with Inspectors {

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
  "MultiParentCasper" should "increment last finalized block as appropriate in round robin" in effectTest {
    val stake      = 10L
    val equalBonds = validators.map(_ -> stake).toMap
    val genesisWithEqualBonds =
      buildGenesis(Seq.empty, equalBonds, 1L, Long.MaxValue, Faucet.noopFaucet, 0L)
    for {
      nodes       <- HashSetCasperTestNode.networkEff(validatorKeys.take(3), genesisWithEqualBonds)
      deployDatas <- (0 to 7).toList.traverse(i => ConstructDeploy.basicDeployData[Effect](i))

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
}
