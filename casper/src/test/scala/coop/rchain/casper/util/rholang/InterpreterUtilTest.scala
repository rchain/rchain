package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString

import InterpreterUtil._
import coop.rchain.casper.{BlockDag, BlockGenerator}
import coop.rchain.casper.BlockDagState._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import org.scalatest.{FlatSpec, Matchers}

import cats.Monad
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._

import java.nio.file.Files

import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.HashMap

class InterpreterUtilTest extends FlatSpec with Matchers with BlockGenerator {

  type StateWithChain[A] = State[BlockDag, A]
  val initState   = BlockDag().copy(currentId = -1)
  val storageSize = 1024L * 1024

  "computeBlockCheckpoint" should "compute the final post-state of a chain properly" in {
    val storageDirectory = Files.createTempDirectory("casper-interp-util-test")

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

    val (genCh, genMap) = computeBlockCheckpoint(genesis,
                                                 genesis,
                                                 chain,
                                                 storageDirectory,
                                                 storageSize,
                                                 Map.empty[ByteString, Checkpoint])
    val genPostState = genCh.toTuplespace.storageRepr
    genPostState.contains("@{2}!(2)") should be(true)
    genPostState.contains("@{123}!(5)") should be(true)

    val (b1Ch, b1Map) =
      computeBlockCheckpoint(b1, genesis, chain, storageDirectory, storageSize, genMap)
    val b1PostState = b1Ch.toTuplespace.storageRepr
    b1PostState.contains("@{1}!(1)") should be(true)
    b1PostState.contains("@{123}!(5)") should be(true)
    b1PostState.contains("@{456}!(10)") should be(true)

    //note skipping of b2 to force a test of the recursive aspect of computeBlockCheckpoint

    val (b3Ch, _) =
      computeBlockCheckpoint(b3, genesis, chain, storageDirectory, storageSize, b1Map)
    val b3PostState = b3Ch.toTuplespace.storageRepr
    b3PostState.contains("@{1}!(1)") should be(true)
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{7}!(7)") should be(true)
  }

  "computeBlockCheckpoint" should "merge histories in case of multiple parents" in {
    val storageDirectory = Files.createTempDirectory("casper-interp-util-test")

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
    val (b3Ch, _) = computeBlockCheckpoint(b3,
                                           genesis,
                                           chain,
                                           storageDirectory,
                                           storageSize,
                                           Map.empty[ByteString, Checkpoint])
    val b3PostState = b3Ch.toTuplespace.storageRepr
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{5}!(5)") should be(true)
    b3PostState.contains("@{6}!(6)") should be(true)
  }
}
