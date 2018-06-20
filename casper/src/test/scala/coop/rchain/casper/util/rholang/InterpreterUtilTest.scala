package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import InterpreterUtil._
import coop.rchain.catscontrib.Capture._
import coop.rchain.casper.{BlockDag, BlockGenerator}
import coop.rchain.casper.BlockDagState._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.rholang.interpreter.Runtime
import org.scalatest.{FlatSpec, Matchers}
import cats.Monad
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._
import java.nio.file.Files

import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.HashMap
import scala.concurrent.SyncVar

class InterpreterUtilTest extends FlatSpec with Matchers with BlockGenerator {

  type StateWithChain[A] = State[BlockDag, A]
  val initState        = BlockDag().copy(currentId = -1)
  val storageSize      = 1024L * 1024
  val storageDirectory = Files.createTempDirectory("casper-interp-util-test")
  val runtime          = new SyncVar[Runtime]()
  runtime.put(Runtime.create(storageDirectory, storageSize))
  val (initStateHash, runtimeManager) = RuntimeManager.fromRuntime(runtime)
  val knownStateHashes                = Set[StateHash](initStateHash)

  "computeBlockCheckpoint" should "compute the final post-state of a chain properly" in {
    val genesisDeploys = Vector(
      "@1!(1)",
      "@2!(2)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)

    val b1Deploys = Vector(
      "@1!(1)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)

    val b2Deploys = Vector(
      "for(@a <- @123; @b <- @456){ @1!(a + b) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)

    val b3Deploys = Vector(
      "@7!(7)"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    /*
     * DAG Looks like this:
     *
     *          b3
     *           |
     *          b2
     *           |
     *          b1
     *           |
     *         genesis
     */
    def createChain[F[_]: Monad: BlockDagState]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploys)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1Deploys)
        b2      <- createBlock[F](Seq(b1.blockHash), deploys = b2Deploys)
        b3      <- createBlock[F](Seq(b2.blockHash), deploys = b3Deploys)
      } yield b3
    val chain   = createChain[StateWithChain].runS(initState).value
    val genesis = chain.idToBlocks(0)

    val b1 = chain.idToBlocks(1)
    val b3 = chain.idToBlocks(3)

    val (postGenStateHash, _) = computeBlockCheckpoint(genesis,
                                                       genesis,
                                                       chain,
                                                       initStateHash,
                                                       knownStateHashes,
                                                       runtimeManager)
    val genPostState = runtimeManager.storageRepr(postGenStateHash)

    genPostState.contains("@{2}!(2)") should be(true)
    genPostState.contains("@{123}!(5)") should be(true)

    val (postB1StateHash, _) =
      computeBlockCheckpoint(b1, genesis, chain, initStateHash, knownStateHashes, runtimeManager)
    val b1PostState = runtimeManager.storageRepr(postB1StateHash)
    b1PostState.contains("@{1}!(1)") should be(true)
    b1PostState.contains("@{123}!(5)") should be(true)
    b1PostState.contains("@{456}!(10)") should be(true)

    //note skipping of b2 to force a test of the recursive aspect of computeBlockCheckpoint

    val (postb3StateHash, _) =
      computeBlockCheckpoint(b3, genesis, chain, initStateHash, knownStateHashes, runtimeManager)
    val b3PostState = runtimeManager.storageRepr(postb3StateHash)
    b3PostState.contains("@{1}!(1)") should be(true)
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{7}!(7)") should be(true)
  }

  it should "merge histories in case of multiple parents" in {
    val genesisDeploys = Vector(
      "@1!(1)",
      "@2!(2)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)

    val b1Deploys = Vector(
      "@5!(5)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)

    val b2Deploys = Vector(
      "@6!(6)"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)

    val b3Deploys = Vector(
      "for(@a <- @123; @b <- @456){ @1!(a + b) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)

    /*
     * DAG Looks like this:
     *
     *           b3
     *          /  \
     *        b1    b2
     *         \    /
     *         genesis
     */
    def createChain[F[_]: Monad: BlockDagState]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploys)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1Deploys)
        b2      <- createBlock[F](Seq(genesis.blockHash), deploys = b2Deploys)
        b3      <- createBlock[F](Seq(b1.blockHash, b2.blockHash), deploys = b3Deploys)
      } yield b3
    val chain   = createChain[StateWithChain].runS(initState).value
    val genesis = chain.idToBlocks(0)

    val b3 = chain.idToBlocks(3)
    val (postb3StateHash, _) =
      computeBlockCheckpoint(b3, genesis, chain, initStateHash, knownStateHashes, runtimeManager)
    val b3PostState = runtimeManager.storageRepr(postb3StateHash)
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{5}!(5)") should be(true)
    b3PostState.contains("@{6}!(6)") should be(true)
  }

  "validateBlockCheckpoint" should "not return a checkpoint for an invalid block" in {
    val deploys     = Vector("@1!(1)").flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val invalidHash = ByteString.EMPTY

    val chain =
      createBlock[StateWithChain](Seq.empty, deploys = deploys, tsHash = invalidHash)
        .runS(initState)
        .value
    val block = chain.idToBlocks(0)

    val (stateHash, _) =
      validateBlockCheckpoint(block, block, chain, initStateHash, knownStateHashes, runtimeManager)

    stateHash should be(None)
  }

  "validateBlockCheckpoint" should "return a checkpoint with the right hash for a valid block" in {
    val deploys = Vector("@1!(1)").flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val (computedTsHash, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               initStateHash,
                               knownStateHashes,
                               runtimeManager)

    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty, deploys = deploys, tsHash = computedTsHash)
        .runS(initState)
        .value
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block, block, chain, initStateHash, knownStateHashes, runtimeManager)

    tsHash should be(Some(computedTsHash))
  }
}
