package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import InterpreterUtil._
import coop.rchain.catscontrib.Capture._
import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{EventConverter, ProtoUtil}
import coop.rchain.rholang.interpreter.Runtime
import org.scalatest.{FlatSpec, Matchers}
import cats.{Id, Monad}
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._
import java.nio.file.Files

import coop.rchain.casper.helper.BlockGenerator
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.rholang.collection.LinkedList
import coop.rchain.shared.Time
import coop.rchain.rspace.trace.Event
import coop.rchain.rspace.trace.Event._
import coop.rchain.shared.AttemptOps._
import monix.execution.Scheduler.Implicits.global
import scodec.Codec

import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.concurrent.SyncVar

class InterpreterUtilTest extends FlatSpec with Matchers with BlockGenerator {
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
    def createChain[F[_]: Monad: BlockDagState: Time]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploys)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1Deploys)
        b2      <- createBlock[F](Seq(b1.blockHash), deploys = b2Deploys)
        b3      <- createBlock[F](Seq(b2.blockHash), deploys = b3Deploys)
      } yield b3
    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)

    val (postGenStateHash, postGenKnownStateHashes) =
      computeBlockCheckpoint(genesis,
                             genesis,
                             chain,
                             initStateHash,
                             knownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedGen = injectPostStateHash(chain, 0, genesis, postGenStateHash)
    val genPostState        = runtimeManager.storageRepr(postGenStateHash)

    genPostState.contains("@{2}!(2)") should be(true)
    genPostState.contains("@{123}!(5)") should be(true)

    val b1 = chainWithUpdatedGen.idToBlocks(1)
    val (postB1StateHash, postB1KnownStateHashes) =
      computeBlockCheckpoint(b1,
                             genesis,
                             chainWithUpdatedGen,
                             initStateHash,
                             postGenKnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB1 = injectPostStateHash(chainWithUpdatedGen, 1, b1, postB1StateHash)
    val b1PostState        = runtimeManager.storageRepr(postB1StateHash)
    b1PostState.contains("@{1}!(1)") should be(true)
    b1PostState.contains("@{123}!(5)") should be(true)
    b1PostState.contains("@{456}!(10)") should be(true)

    val b2 = chainWithUpdatedB1.idToBlocks(2)
    val (postB2StateHash, postB2KnownStateHashes) =
      computeBlockCheckpoint(b2,
                             genesis,
                             chainWithUpdatedB1,
                             initStateHash,
                             postB1KnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB2 = injectPostStateHash(chainWithUpdatedB1, 2, b2, postB2StateHash)

    val b3 = chainWithUpdatedB2.idToBlocks(3)
    val (postb3StateHash, _) =
      computeBlockCheckpoint(b3,
                             genesis,
                             chainWithUpdatedB2,
                             initStateHash,
                             postB2KnownStateHashes,
                             runtimeManager.computeState)
    val b3PostState = runtimeManager.storageRepr(postb3StateHash)
    b3PostState.contains("@{1}!(1)") should be(true)
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{7}!(7)") should be(true)
  }

  private def injectPostStateHash(chain: BlockDag,
                                  id: Int,
                                  b: BlockMessage,
                                  postGenStateHash: StateHash) = {
    val updatedBlockPostState = b.body.get.postState.get.withTuplespace(postGenStateHash)
    val updatedBlockBody      = b.body.get.withPostState(updatedBlockPostState)
    val updatedBlock          = b.withBody(updatedBlockBody)
    chain.copy(idToBlocks = chain.idToBlocks.updated(id, updatedBlock),
               blockLookup = chain.blockLookup.updated(b.blockHash, updatedBlock))
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
    def createChain[F[_]: Monad: BlockDagState: Time]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploys)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1Deploys)
        b2      <- createBlock[F](Seq(genesis.blockHash), deploys = b2Deploys)
        b3      <- createBlock[F](Seq(b1.blockHash, b2.blockHash), deploys = b3Deploys)
      } yield b3
    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)
    val (postGenStateHash, postGenKnownStateHashes) =
      computeBlockCheckpoint(genesis,
                             genesis,
                             chain,
                             initStateHash,
                             knownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedGen = injectPostStateHash(chain, 0, genesis, postGenStateHash)
    val b1                  = chainWithUpdatedGen.idToBlocks(1)
    val (postB1StateHash, postB1KnownStateHashes) =
      computeBlockCheckpoint(b1,
                             genesis,
                             chainWithUpdatedGen,
                             initStateHash,
                             postGenKnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB1 = injectPostStateHash(chainWithUpdatedGen, 1, b1, postB1StateHash)
    val b2                 = chainWithUpdatedB1.idToBlocks(2)
    val (postB2StateHash, postB2KnownStateHashes) =
      computeBlockCheckpoint(b2,
                             genesis,
                             chainWithUpdatedB1,
                             initStateHash,
                             postB1KnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB2 = injectPostStateHash(chainWithUpdatedB1, 2, b2, postB2StateHash)
    val updatedGenesis     = chainWithUpdatedB2.idToBlocks(0)
    val b3                 = chainWithUpdatedB2.idToBlocks(3)
    val (postb3StateHash, _) =
      computeBlockCheckpoint(b3,
                             updatedGenesis,
                             chainWithUpdatedB2,
                             initStateHash,
                             postB2KnownStateHashes,
                             runtimeManager.computeState)
    val b3PostState = runtimeManager.storageRepr(postb3StateHash)
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{5}!(5)") should be(true)
    b3PostState.contains("@{6}!(6)") should be(true)
  }

  "validateBlockCheckpoint" should "not return a checkpoint for an invalid block" in {
    val deploys = Vector("@1!(1)").flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val (computedTsCheckpoint, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               initStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val invalidHash   = ByteString.EMPTY

    val chain =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploys,
                                  tsHash = invalidHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (stateHash, _) =
      validateBlockCheckpoint(block, block, chain, initStateHash, knownStateHashes, runtimeManager)

    stateHash should be(None)
  }

  "validateBlockCheckpoint" should "return a checkpoint with the right hash for a valid block" in {
    val deploys =
      Vector("@1!(1)",
             "@2!(1)",
             "@2!(2)",
             "@2!(3)",
             "@2!(4)",
             "@2!(5)",
             "for (@x <- @1) { @2!(x) }",
             "for (@x <- @2) { @3!(x) }")
        .flatMap(mkTerm(_).toOption)
        .map(ProtoUtil.termDeploy)
    val (computedTsCheckpoint, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               initStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploys,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block, block, chain, initStateHash, knownStateHashes, runtimeManager)

    tsHash should be(Some(computedTsHash))
  }

  "validateBlockCheckpoint" should "pass linked list test" in {
    val deploys = Vector(
      """
        |contract @"recursionTest"(@list) = {
        |  new loop in {
        |    contract loop(@rem, @acc) = {
        |      match rem {
        |        [head, ...tail] => {
        |          new newAccCh in {
        |            newAccCh!([head, acc]) |
        |            for(@newAcc <- newAccCh) {
        |              loop!(tail, newAcc)
        |            }
        |          }
        |        }
        |        _ => { Nil } // Normally we would print the "acc" ([2,[1,[]]]) out
        |      }
        |    } |
        |    new unusedCh in {
        |      loop!(list, [])
        |    }
        |  }
        |} |
        |@"recursionTest"!([1,2])
      """.stripMargin
    ).map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))
    val (computedTsCheckpoint, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               initStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploys,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block, block, chain, initStateHash, knownStateHashes, runtimeManager)

    tsHash should be(Some(computedTsHash))
  }

  "validateBlockCheckpoint" should "pass persistent produce test with causality" in {
    val deploys =
      Vector("""new x, y, delay in {
              contract delay(@n) = {
                if (n < 100) {
                  delay!(n + 1)
                } else {
                  x!!(1)
                }
              } |
              delay!(0) |
              y!(0) |
              for (_ <- x; @0 <- y) { y!(1) } |
              for (_ <- x; @1 <- y) { y!(2) } |
              for (_ <- x; @2 <- y) { y!(3) } |
              for (_ <- x; @3 <- y) { y!(4) } |
              for (_ <- x; @4 <- y) { y!(5) } |
              for (_ <- x; @5 <- y) { y!(6) } |
              for (_ <- x; @6 <- y) { y!(7) } |
              for (_ <- x; @7 <- y) { y!(8) } |
              for (_ <- x; @8 <- y) { y!(9) } |
              for (_ <- x; @9 <- y) { y!(10) } |
              for (_ <- x; @10 <- y) { y!(11) } |
              for (_ <- x; @11 <- y) { y!(12) } |
              for (_ <- x; @12 <- y) { y!(13) } |
              for (_ <- x; @13 <- y) { y!(14) } |
              for (_ <- x; @14 <- y) { Nil }
             }
          """)
        .map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))
    val (computedTsCheckpoint, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               initStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploys,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block, block, chain, initStateHash, knownStateHashes, runtimeManager)

    tsHash should be(Some(computedTsHash))
  }

  "validateBlockCheckpoint" should "pass tests involving primitives" in {
    val deploys =
      Vector(
        """
          |new loop, primeCheck in {
          |  contract loop(@x) = {
          |    match x {
          |      [] => Nil
          |      [head ...tail] => {
          |        new ret in {
          |          for (_ <- ret) {
          |            loop!(tail)
          |          } | primeCheck!(head, *ret)
          |        }
          |      }
          |    }
          |  } |
          |  contract primeCheck(@x, ret) = {
          |    match x {
          |      Nil => @"stdoutAck"!("Nil", *ret)
          |      ~{~Nil | ~Nil} => @"stdoutAck"!("Prime", *ret)
          |      _ => @"stdoutAck"!("Composite", *ret)
          |    }
          |  } |
          |  loop!([Nil, 7, 7 | 8, 9 | Nil, 9 | 10, Nil, 9])
          |}""".stripMargin
      ).map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))
    val (computedTsCheckpoint, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               initStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploys,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block, block, chain, initStateHash, knownStateHashes, runtimeManager)

    tsHash should be(Some(computedTsHash))
  }
}
