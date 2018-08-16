package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import InterpreterUtil._
import coop.rchain.catscontrib.Capture._
import coop.rchain.casper.{BlockDag, MultiParentCasperInstances}
import coop.rchain.casper.protocol.{Event => CasperEvent, _}
import coop.rchain.casper.util.{EventConverter, ProtoUtil}
import coop.rchain.rholang.interpreter.Runtime
import org.scalatest.{FlatSpec, Matchers}
import cats.{Id, Monad}
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._
import java.nio.file.Files

import cats.effect.Bracket
import cats.mtl.MonadState
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.helper.{BlockGenerator, BlockStoreFixture, BlockStoreTestFixture}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.util.rholang.RuntimeManager.{DeployError, StateHash}
import coop.rchain.models.PCost
import coop.rchain.rholang.collection.LinkedList
import coop.rchain.rspace.Checkpoint
import coop.rchain.shared.Time
import coop.rchain.rspace.trace.Event
import coop.rchain.rspace.trace.Event._
import coop.rchain.shared.AttemptOps._
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import scodec.Codec

import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.concurrent.SyncVar

class InterpreterUtilTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture {
  val initState        = BlockDag().copy(currentId = -1)
  val storageSize      = 1024L * 1024
  val storageDirectory = Files.createTempDirectory("casper-interp-util-test")
  val activeRuntime    = Runtime.create(storageDirectory, storageSize)
  val runtimeManager   = RuntimeManager.fromRuntime(activeRuntime)
  val emptyStateHash   = runtimeManager.emptyStateHash
  val knownStateHashes = Set[StateHash](emptyStateHash)

  private def computeBlockCheckpoint(
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      defaultStateHash: StateHash,
      knownStateHashes: Set[StateHash],
      computeState: (StateHash,
                     Seq[Deploy]) => Either[DeployError, (Checkpoint, Vector[DeployCost])])
    : (StateHash, Seq[CasperEvent], Set[StateHash], Vector[DeployCost]) = {
    val (checkpoint, mergeLog, updatedKnownStateHashes, deployCost) =
      InterpreterUtil.computeBlockCheckpointFromDeploys(b,
                                                        genesis,
                                                        dag,
                                                        BlockStore[Id].asMap(),
                                                        defaultStateHash,
                                                        knownStateHashes,
                                                        computeState)
    val blockStateHash = ByteString.copyFrom(checkpoint.root.bytes.toArray)
    (blockStateHash, mergeLog, updatedKnownStateHashes, deployCost)
  }

  "computeBlockCheckpoint" should "compute the final post-state of a chain properly" in {
    val genesisDeploys = Vector(
      "@1!(1)",
      "@2!(2)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val genesisDeploysCost =
      genesisDeploys.map(d => DeployCost().withDeploy(d).withCost(PCost(0L, 1)))

    val b1Deploys = Vector(
      "@1!(1)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val b1DeploysCost = b1Deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val b2Deploys = Vector(
      "for(@a <- @123; @b <- @456){ @1!(a + b) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val b2DeploysCost = b2Deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val b3Deploys = Vector(
      "@7!(7)"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val b3DeploysCost = b3Deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

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
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploysCost)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1DeploysCost)
        b2      <- createBlock[F](Seq(b1.blockHash), deploys = b2DeploysCost)
        b3      <- createBlock[F](Seq(b2.blockHash), deploys = b3DeploysCost)
      } yield b3
    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)

    val (postGenStateHash, _, postGenKnownStateHashes, postGenDeployCost) =
      computeBlockCheckpoint(genesis,
                             genesis,
                             chain,
                             emptyStateHash,
                             knownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedGen = injectPostStateHash(chain, 0, genesis, postGenStateHash)
    val genPostState        = runtimeManager.storageRepr(postGenStateHash)

    genPostState.contains("@{2}!(2)") should be(true)
    genPostState.contains("@{123}!(5)") should be(true)

    val b1 = chainWithUpdatedGen.idToBlocks(1)
    val (postB1StateHash, _, postB1KnownStateHashes, postB1DeployCost) =
      computeBlockCheckpoint(b1,
                             genesis,
                             chainWithUpdatedGen,
                             emptyStateHash,
                             postGenKnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB1 = injectPostStateHash(chainWithUpdatedGen, 1, b1, postB1StateHash)
    val b1PostState        = runtimeManager.storageRepr(postB1StateHash)
    b1PostState.contains("@{1}!(1)") should be(true)
    b1PostState.contains("@{123}!(5)") should be(true)
    b1PostState.contains("@{456}!(10)") should be(true)

    val b2 = chainWithUpdatedB1.idToBlocks(2)
    val (postB2StateHash, _, postB2KnownStateHashes, postB2DeployCost) =
      computeBlockCheckpoint(b2,
                             genesis,
                             chainWithUpdatedB1,
                             emptyStateHash,
                             postB1KnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB2 = injectPostStateHash(chainWithUpdatedB1, 2, b2, postB2StateHash)

    val b3 = chainWithUpdatedB2.idToBlocks(3)
    val (postb3StateHash, mergeLog, _, _) =
      computeBlockCheckpoint(b3,
                             genesis,
                             chainWithUpdatedB2,
                             emptyStateHash,
                             postB2KnownStateHashes,
                             runtimeManager.computeState)
    val b3PostState = runtimeManager.storageRepr(postb3StateHash)

    mergeLog.isEmpty shouldBe true
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
    BlockStore[Id].put(b.blockHash, updatedBlock)
    chain.copy(idToBlocks = chain.idToBlocks.updated(id, updatedBlock))
  }

  it should "merge histories in case of multiple parents" in {
    val genesisDeploys = Vector(
      "@1!(1)",
      "@2!(2)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val genesisDeploysWithCost =
      genesisDeploys.map(d => DeployCost().withDeploy(d).withCost(PCost(0L, 1)))

    val b1Deploys = Vector(
      "@5!(5)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val b1DeploysWithCost = b1Deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(2L, 2)))

    val b2Deploys = Vector(
      "@6!(6)"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val b2DeploysWithCost = b2Deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val b3Deploys = Vector(
      "for(@a <- @123; @b <- @456){ @1!(a + b) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val b3DeploysWithCost = b3Deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(5L, 5)))

    /*
     * DAG Looks like this:
     *
     *           b3
     *          /  \
     *        b1    b2
     *         \    /
     *         genesis
     */
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploysWithCost)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
        b2      <- createBlock[F](Seq(genesis.blockHash), deploys = b2DeploysWithCost)
        b3      <- createBlock[F](Seq(b1.blockHash, b2.blockHash), deploys = b3DeploysWithCost)
      } yield b3
    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)
    val (postGenStateHash, _, postGenKnownStateHashes, postGenDeployCost) =
      computeBlockCheckpoint(genesis,
                             genesis,
                             chain,
                             emptyStateHash,
                             knownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedGen = injectPostStateHash(chain, 0, genesis, postGenStateHash)
    val b1                  = chainWithUpdatedGen.idToBlocks(1)
    val (postB1StateHash, _, postB1KnownStateHashes, postB1DeployCost) =
      computeBlockCheckpoint(b1,
                             genesis,
                             chainWithUpdatedGen,
                             emptyStateHash,
                             postGenKnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB1 = injectPostStateHash(chainWithUpdatedGen, 1, b1, postB1StateHash)
    val b2                 = chainWithUpdatedB1.idToBlocks(2)
    val (postB2StateHash, _, postB2KnownStateHashes, postB2DeployCost) =
      computeBlockCheckpoint(b2,
                             genesis,
                             chainWithUpdatedB1,
                             emptyStateHash,
                             postB1KnownStateHashes,
                             runtimeManager.computeState)
    val chainWithUpdatedB2 = injectPostStateHash(chainWithUpdatedB1, 2, b2, postB2StateHash)
    val updatedGenesis     = chainWithUpdatedB2.idToBlocks(0)
    val b3                 = chainWithUpdatedB2.idToBlocks(3)
    val (postb3StateHash, mergeLog, _, postB3DeployCost) =
      computeBlockCheckpoint(b3,
                             updatedGenesis,
                             chainWithUpdatedB2,
                             emptyStateHash,
                             postB2KnownStateHashes,
                             runtimeManager.computeState)
    val b3PostState = runtimeManager.storageRepr(postb3StateHash)

    mergeLog.nonEmpty shouldBe true
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{5}!(5)") should be(true)
    b3PostState.contains("@{6}!(6)") should be(true)
  }

  def computeSingleDeployCost(deploy: Deploy*): Vector[DeployCost] = {
    val (_, _, _, cost) = computeDeploysCheckpoint(Seq.empty,
                                                   deploy,
                                                   BlockMessage(),
                                                   initState,
                                                   BlockStore[Id].asMap(),
                                                   emptyStateHash,
                                                   knownStateHashes,
                                                   runtimeManager.computeState)
    cost
  }

  "computeDeploysCheckpoint" should "aggregate cost of deploying rholang programs within the block" in {
    //reference costs
    //deploy each Rholang program separately and record its cost
    val deploy1 = ProtoUtil.termDeploy(mkTerm("@1!(Nil)").toOption.get)
    val deploy2 = ProtoUtil.termDeploy(mkTerm("@3!([1,2,3,4])").toOption.get)
    val deploy3 =
      ProtoUtil.termDeploy(mkTerm("for(@x <- @0) { @4!(x.toByteArray()) }").toOption.get)

    val cost1 = computeSingleDeployCost(deploy1)
    val cost2 = computeSingleDeployCost(deploy2)
    val cost3 = computeSingleDeployCost(deploy3)

    val accCostsSep = cost1 ++ cost2 ++ cost3

    //cost within the block should be the same as sum of deploying all programs separately
    val singleDeploy = Seq(deploy1, deploy2, deploy3)
    val accCostBatch = computeSingleDeployCost(singleDeploy: _*)

    accCostBatch should contain theSameElementsAs (accCostsSep)
  }

  it should "return cost of deploying even if one of the programs withing the deployment throws an error" in {
    pendingUntilFixed { //reference costs
      //deploy each Rholang program separately and record its cost
      val deploy1 = ProtoUtil.termDeploy(mkTerm("@1!(Nil)").toOption.get)
      val deploy2 = ProtoUtil.termDeploy(mkTerm("@2!([1,2,3,4])").toOption.get)

      val cost1 = computeSingleDeployCost(deploy1)
      val cost2 = computeSingleDeployCost(deploy2)

      val accCostsSep = cost1 ++ cost2

      val deployErr    = ProtoUtil.termDeploy(mkTerm("@3!(\"a\" + 3)").toOption.get)
      val batchDeploy  = Seq(deploy1, deploy2, deployErr)
      val accCostBatch = computeSingleDeployCost(batchDeploy: _*)

      accCostBatch should contain theSameElementsAs (accCostsSep)
    }
  }

  "validateBlockCheckpoint" should "not return a checkpoint for an invalid block" in {
    val deploys     = Vector("@1!(1)").flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeploy)
    val deploysCost = deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val (computedTsCheckpoint, _, _, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               BlockStore[Id].asMap(),
                               emptyStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val invalidHash   = ByteString.EMPTY

    val chain =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploysCost,
                                  tsHash = invalidHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (stateHash, _) =
      validateBlockCheckpoint(block,
                              block,
                              chain,
                              BlockStore[Id].asMap(),
                              emptyStateHash,
                              knownStateHashes,
                              runtimeManager)

    stateHash should be(None)
  }

  it should "return a checkpoint with the right hash for a valid block" in {
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

    val deploysCost = deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val (computedTsCheckpoint, _, _, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               BlockStore[Id].asMap(),
                               emptyStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploysCost,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block,
                              block,
                              chain,
                              BlockStore[Id].asMap(),
                              emptyStateHash,
                              knownStateHashes,
                              runtimeManager)

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

    val deploysCost = deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val (computedTsCheckpoint, _, _, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               BlockStore[Id].asMap(),
                               emptyStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploysCost,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block,
                              block,
                              chain,
                              BlockStore[Id].asMap(),
                              emptyStateHash,
                              knownStateHashes,
                              runtimeManager)

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

    val deploysCost = deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val (computedTsCheckpoint, _, _, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               BlockStore[Id].asMap(),
                               emptyStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploysCost,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block,
                              block,
                              chain,
                              BlockStore[Id].asMap(),
                              emptyStateHash,
                              knownStateHashes,
                              runtimeManager)

    tsHash should be(Some(computedTsHash))
  }

  "validateBlockCheckpoint" should "pass tests involving primitives" in {
    val deploys =
      Vector(
        """
          |new loop, primeCheck, stdoutAck(`rho:io:stdoutAck`) in {
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
          |      Nil => stdoutAck!("Nil", *ret)
          |      ~{~Nil | ~Nil} => stdoutAck!("Prime", *ret)
          |      _ => stdoutAck!("Composite", *ret)
          |    }
          |  } |
          |  loop!([Nil, 7, 7 | 8, 9 | Nil, 9 | 10, Nil, 9])
          |}""".stripMargin
      ).map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))

    val deploysCost = deploys.map(d => DeployCost().withDeploy(d).withCost(PCost(1L, 1)))

    val (computedTsCheckpoint, _, _, _) =
      computeDeploysCheckpoint(Seq.empty,
                               deploys,
                               BlockMessage(),
                               initState,
                               BlockStore[Id].asMap(),
                               emptyStateHash,
                               knownStateHashes,
                               runtimeManager.computeState)
    val computedTsLog  = computedTsCheckpoint.log.map(EventConverter.toCasperEvent)
    val computedTsHash = ByteString.copyFrom(computedTsCheckpoint.root.bytes.toArray)
    val chain: BlockDag =
      createBlock[StateWithChain](Seq.empty,
                                  deploys = deploysCost,
                                  tsHash = computedTsHash,
                                  tsLog = computedTsLog)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val (tsHash, _) =
      validateBlockCheckpoint(block,
                              block,
                              chain,
                              BlockStore[Id].asMap(),
                              emptyStateHash,
                              knownStateHashes,
                              runtimeManager)

    tsHash should be(Some(computedTsHash))
  }
}
