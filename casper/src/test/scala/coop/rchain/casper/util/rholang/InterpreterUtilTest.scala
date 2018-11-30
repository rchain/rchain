package coop.rchain.casper.util.rholang

import cats.mtl.implicits._
import cats.{Id, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.BlockDag
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil._
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.models.PCost
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.Time
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class InterpreterUtilTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture {
  val initState: IndexedBlockDag = IndexedBlockDag.empty.copy(currentId = -1)

  implicit val logEff: LogStub[Id] = new LogStub[Id]

  private def computeBlockCheckpoint(
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      runtimeManager: RuntimeManager
  ): (StateHash, Seq[ProcessedDeploy]) = {
    val Right((preStateHash, postStateHash, processedDeploys)) =
      InterpreterUtil
        .computeBlockCheckpointFromDeploys[Id](b, genesis, dag, runtimeManager)

    (postStateHash, processedDeploys.map(ProcessedDeployUtil.fromInternal))
  }

  "computeBlockCheckpoint" should "compute the final post-state of a chain properly" in {
    val genesisDeploys = Vector(
      "@1!(1)",
      "@2!(2)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).flatMap(mkTerm(_).toOption)
      .map(ProtoUtil.termDeploy(_, System.currentTimeMillis(), accounting.MAX_VALUE))
    val genesisDeploysCost =
      genesisDeploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1)))

    val b1Deploys = Vector(
      "@1!(1)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val b1DeploysCost = b1Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1, 1L)))

    val b2Deploys = Vector(
      "for(@a <- @123; @b <- @456){ @1!(a + b) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val b2DeploysCost = b2Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1, 1L)))

    val b3Deploys = Vector(
      "@7!(7)"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val b3DeploysCost = b3Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1, 1L)))

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
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] = {
      import cats.implicits._

      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploysCost)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1DeploysCost)
        b2      <- createBlock[F](Seq(b1.blockHash), deploys = b2DeploysCost)
        b3      <- createBlock[F](Seq(b2.blockHash), deploys = b3DeploysCost)
      } yield b3
    }
    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)

    val (genPostState, b1PostState, b3PostState) =
      mkRuntimeManager("interpreter-util-test")
        .use { runtimeManager =>
          Task.delay {
            val (postGenStateHash, postGenProcessedDeploys) =
              computeBlockCheckpoint(genesis, genesis, chain, runtimeManager)
            val chainWithUpdatedGen =
              injectPostStateHash(chain, 0, genesis, postGenStateHash, postGenProcessedDeploys)
            val genPostState = runtimeManager.storageRepr(postGenStateHash).get

            val b1 = chainWithUpdatedGen.idToBlocks(1)
            val (postB1StateHash, postB1ProcessedDeploys) =
              computeBlockCheckpoint(
                b1,
                genesis,
                chainWithUpdatedGen,
                runtimeManager
              )
            val chainWithUpdatedB1 =
              injectPostStateHash(
                chainWithUpdatedGen,
                1,
                b1,
                postB1StateHash,
                postB1ProcessedDeploys
              )
            val b1PostState = runtimeManager.storageRepr(postB1StateHash).get

            val b2 = chainWithUpdatedB1.idToBlocks(2)
            val (postB2StateHash, postB2ProcessedDeploys) =
              computeBlockCheckpoint(
                b2,
                genesis,
                chainWithUpdatedB1,
                runtimeManager
              )
            val chainWithUpdatedB2 =
              injectPostStateHash(
                chainWithUpdatedB1,
                2,
                b2,
                postB2StateHash,
                postB2ProcessedDeploys
              )

            val b3 = chainWithUpdatedB2.idToBlocks(3)
            val (postb3StateHash, _) =
              computeBlockCheckpoint(
                b3,
                genesis,
                chainWithUpdatedB2,
                runtimeManager
              )
            val b3PostState = runtimeManager.storageRepr(postb3StateHash).get

            (genPostState, b1PostState, b3PostState)
          }
        }
        .runSyncUnsafe(10.seconds)

    genPostState.contains("@{2}!(2)") should be(true)
    genPostState.contains("@{123}!(5)") should be(true)

    b1PostState.contains("@{1}!(1)") should be(true)
    b1PostState.contains("@{123}!(5)") should be(true)
    b1PostState.contains("@{456}!(10)") should be(true)

    b3PostState.contains("@{1}!(1)") should be(true)
    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{7}!(7)") should be(true)
  }

  private def injectPostStateHash(
      chain: IndexedBlockDag,
      id: Int,
      b: BlockMessage,
      postGenStateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ) = {
    val updatedBlockPostState = b.getBody.getState.withPostStateHash(postGenStateHash)
    val updatedBlockBody =
      b.getBody.withState(updatedBlockPostState).withDeploys(processedDeploys)
    val updatedBlock = b.withBody(updatedBlockBody)
    BlockStore[Id].put(b.blockHash, updatedBlock)
    chain.copy(idToBlocks = chain.idToBlocks.updated(id, updatedBlock))
  }

  it should "merge histories in case of multiple parents" in {
    val genesisDeploys = Vector(
      "@1!(1)",
      "@2!(2)",
      "for(@a <- @1){ @123!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val genesisDeploysWithCost =
      genesisDeploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1)))

    val b1Deploys = Vector(
      "@5!(5)",
      "for(@a <- @2){ @456!(5 * a) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val b1DeploysWithCost =
      b1Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(2, 2L)))

    val b2Deploys = Vector(
      "@6!(6)"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val b2DeploysWithCost =
      b2Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1, 1L)))

    val b3Deploys = Vector(
      "for(@a <- @123; @b <- @456){ @1!(a + b) }"
    ).flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val b3DeploysWithCost =
      b3Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(5, 5L)))

    /*
     * DAG Looks like this:
     *
     *           b3
     *          /  \
     *        b1    b2
     *         \    /
     *         genesis
     */
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] = {
      import cats.implicits._

      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploysWithCost)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
        b2      <- createBlock[F](Seq(genesis.blockHash), deploys = b2DeploysWithCost)
        b3      <- createBlock[F](Seq(b1.blockHash, b2.blockHash), deploys = b3DeploysWithCost)
      } yield b3
    }

    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)

    val b3PostState = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          val (postGenStateHash, postGenProcessedDeploys) =
            computeBlockCheckpoint(genesis, genesis, chain, runtimeManager)
          val chainWithUpdatedGen =
            injectPostStateHash(chain, 0, genesis, postGenStateHash, postGenProcessedDeploys)
          val b1 = chainWithUpdatedGen.idToBlocks(1)
          val (postB1StateHash, postB1ProcessedDeploys) =
            computeBlockCheckpoint(
              b1,
              genesis,
              chainWithUpdatedGen,
              runtimeManager
            )
          val chainWithUpdatedB1 =
            injectPostStateHash(chainWithUpdatedGen, 1, b1, postB1StateHash, postB1ProcessedDeploys)
          val b2 = chainWithUpdatedB1.idToBlocks(2)
          val (postB2StateHash, postB2ProcessedDeploys) =
            computeBlockCheckpoint(
              b2,
              genesis,
              chainWithUpdatedB1,
              runtimeManager
            )
          val chainWithUpdatedB2 =
            injectPostStateHash(chainWithUpdatedB1, 2, b2, postB2StateHash, postB2ProcessedDeploys)
          val updatedGenesis = chainWithUpdatedB2.idToBlocks(0)
          val b3             = chainWithUpdatedB2.idToBlocks(3)
          val (postb3StateHash, _) =
            computeBlockCheckpoint(
              b3,
              updatedGenesis,
              chainWithUpdatedB2,
              runtimeManager
            )
          runtimeManager.storageRepr(postb3StateHash).get
        }
      }
      .runSyncUnsafe(10.seconds)

    b3PostState.contains("@{1}!(15)") should be(true)
    b3PostState.contains("@{5}!(5)") should be(true)
    b3PostState.contains("@{6}!(6)") should be(true)
  }

  val registry =
    """
    |new ri(`rho:registry:insertArbitrary`) in {
    |  new X, Y in {
    |    ri!(*X, *Y)
    |  }
    |}
  """.stripMargin

  val other =
    """
    |new helloWorld, stdout(`rho:io:stdout`), stdoutAck(`rho:io:stdoutAck`) in {
    |  contract helloWorld(@name) = {
    |    new ack in {
    |      stdoutAck!("Hello, ", *ack) |
    |      for (_ <- ack) {
    |        stdoutAck!(name, *ack) |
    |        for (_ <- ack) {
    |          stdout!("\n")
    |        }
    |      }
    |    }
    |  } |
    |  helloWorld!("Joe")
    |}
    |
  """.stripMargin

  def prepareDeploys(v: Vector[String], c: PCost) = {
    val genesisDeploys = v.flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    genesisDeploys.map(d => ProcessedDeploy().withDeploy(d).withCost(c))
  }

  it should "merge histories in case of multiple parents with complex contract" ignore {

    val contract = registry

    val genesisDeploysWithCost = prepareDeploys(Vector.empty, PCost(1))
    val b1DeploysWithCost      = prepareDeploys(Vector(contract), PCost(2, 2L))
    val b2DeploysWithCost      = prepareDeploys(Vector(contract), PCost(1, 1L))
    val b3DeploysWithCost      = prepareDeploys(Vector.empty, PCost(5, 5L))

    /*
     * DAG Looks like this:
     *
     *           b3
     *          /  \
     *        b1    b2
     *         \    /
     *         genesis
     */
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] = {
      import cats.implicits._

      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploysWithCost)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
        b2      <- createBlock[F](Seq(genesis.blockHash), deploys = b2DeploysWithCost)
        b3      <- createBlock[F](Seq(b1.blockHash, b2.blockHash), deploys = b3DeploysWithCost)
      } yield b3
    }

    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)

    val postState = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          def step(chain: IndexedBlockDag, index: Int, genesis: BlockMessage) = {
            val b1 = chain.idToBlocks(index)
            val (postB1StateHash, postB1ProcessedDeploys) =
              computeBlockCheckpoint(
                b1,
                genesis,
                chain,
                runtimeManager
              )
            injectPostStateHash(chain, index, b1, postB1StateHash, postB1ProcessedDeploys)
          }

          val chainWithUpdatedGen = step(chain, 0, genesis)
          val chainWithUpdatedB1  = step(chainWithUpdatedGen, 1, genesis)
          val chainWithUpdatedB2  = step(chainWithUpdatedB1, 2, genesis)
          val b3                  = chainWithUpdatedB2.idToBlocks(3)
          validateBlockCheckpoint[Id](b3, chain, runtimeManager)
        }
      }
      .runSyncUnsafe(10.seconds)

    postState shouldBe Right(None)
  }

  it should "merge histories in case of multiple parents (uneven histories)" ignore {
    val contract = registry

    val genesisDeploysWithCost = prepareDeploys(Vector(contract), PCost(1))

    val b1DeploysWithCost = prepareDeploys(Vector(contract), PCost(2, 2L))

    val b2DeploysWithCost = prepareDeploys(Vector(contract), PCost(1, 1L))

    val b3DeploysWithCost = prepareDeploys(Vector(contract), PCost(5, 5L))

    val b4DeploysWithCost = prepareDeploys(Vector(contract), PCost(5, 5L))

    val b5DeploysWithCost = prepareDeploys(Vector(contract), PCost(5, 5L))

    /*
     * DAG Looks like this:
     *
     *           b5
     *          /  \
     *         |    |
     *         |    b4
     *         |    |
     *        b2    b3
     *         \    /
     *          \  /
     *           |
     *           b1
     *           |
     *         genesis
     */
    def createChain[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] = {
      import cats.implicits._

      for {
        genesis <- createBlock[F](Seq.empty, deploys = genesisDeploysWithCost)
        b1      <- createBlock[F](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
        b2      <- createBlock[F](Seq(b1.blockHash), deploys = b2DeploysWithCost)
        b3      <- createBlock[F](Seq(b1.blockHash), deploys = b3DeploysWithCost)
        b4      <- createBlock[F](Seq(b3.blockHash), deploys = b4DeploysWithCost)
        b5      <- createBlock[F](Seq(b2.blockHash, b4.blockHash), deploys = b5DeploysWithCost)
      } yield b5
    }

    val chain   = createChain[StateWithChain].runS(initState)
    val genesis = chain.idToBlocks(0)

    val postState = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {

          def step(chain: IndexedBlockDag, index: Int, genesis: BlockMessage) = {
            val b1 = chain.idToBlocks(index)
            val (postB1StateHash, postB1ProcessedDeploys) =
              computeBlockCheckpoint(
                b1,
                genesis,
                chain,
                runtimeManager
              )
            injectPostStateHash(chain, index, b1, postB1StateHash, postB1ProcessedDeploys)
          }

          val (postGenStateHash, postGenProcessedDeploys) =
            computeBlockCheckpoint(genesis, genesis, chain, runtimeManager)
          val chainWithUpdatedGen =
            injectPostStateHash(chain, 0, genesis, postGenStateHash, postGenProcessedDeploys)

          val chainWithUpdatedB1 = step(chainWithUpdatedGen, 1, genesis)

          val chainWithUpdatedB2 = step(chainWithUpdatedB1, 2, genesis)

          val chainWithUpdatedB3 = step(chainWithUpdatedB2, 3, genesis)

          val chainWithUpdatedB4 = step(chainWithUpdatedB3, 4, genesis)

          validateBlockCheckpoint[Id](chainWithUpdatedB4.idToBlocks(5), chain, runtimeManager)
        }
      }
      .runSyncUnsafe(10.seconds)

    postState shouldBe Right(None)
  }

  def computeSingleProcessedDeploy(
      runtimeManager: RuntimeManager,
      deploy: Deploy*
  ): Seq[InternalProcessedDeploy] = {
    val Right((_, _, result)) =
      computeDeploysCheckpoint[Id](Seq.empty, deploy, initState, runtimeManager)
    result
  }

  "computeDeploysCheckpoint" should "aggregate cost of deploying rholang programs within the block" in {
    //reference costs
    //deploy each Rholang program separately and record its cost
    val deploy1 = ProtoUtil.termDeploy(
      mkTerm("@1!(Nil)").toOption.get,
      System.currentTimeMillis(),
      accounting.MAX_VALUE
    )
    val deploy2 =
      ProtoUtil.termDeploy(
        mkTerm("@3!([1,2,3,4])").toOption.get,
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )
    val deploy3 =
      ProtoUtil.termDeploy(
        mkTerm("for(@x <- @0) { @4!(x.toByteArray()) }").toOption.get,
        System.currentTimeMillis(),
        accounting.MAX_VALUE
      )

    val (accCostBatch, accCostsSep) = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          val cost1 = computeSingleProcessedDeploy(runtimeManager, deploy1)
          val cost2 = computeSingleProcessedDeploy(runtimeManager, deploy2)
          val cost3 = computeSingleProcessedDeploy(runtimeManager, deploy3)

          val accCostsSep = cost1 ++ cost2 ++ cost3

          //cost within the block should be the same as sum of deploying all programs separately
          val singleDeploy = Seq(deploy1, deploy2, deploy3)
          val accCostBatch = computeSingleProcessedDeploy(runtimeManager, singleDeploy: _*)

          (accCostBatch, accCostsSep)
        }
      }
      .runSyncUnsafe(10.seconds)

    accCostBatch should contain theSameElementsAs accCostsSep
  }

  it should "return cost of deploying even if one of the programs withing the deployment throws an error" in {
    pendingUntilFixed { //reference costs
      //deploy each Rholang program separately and record its cost
      val deploy1 =
        ProtoUtil.termDeploy(
          mkTerm("@1!(Nil)").toOption.get,
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )
      val deploy2 =
        ProtoUtil.termDeploy(
          mkTerm("@2!([1,2,3,4])").toOption.get,
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )

      val (accCostBatch, accCostsSep) = mkRuntimeManager("interpreter-util-test")
        .use { runtimeManager =>
          Task.delay {
            val cost1 = computeSingleProcessedDeploy(runtimeManager, deploy1)
            val cost2 = computeSingleProcessedDeploy(runtimeManager, deploy2)

            val accCostsSep = cost1 ++ cost2

            val deployErr =
              ProtoUtil.termDeploy(
                mkTerm("@3!(\"a\" + 3)").toOption.get,
                System.currentTimeMillis(),
                accounting.MAX_VALUE
              )
            val batchDeploy  = Seq(deploy1, deploy2, deployErr)
            val accCostBatch = computeSingleProcessedDeploy(runtimeManager, batchDeploy: _*)

            (accCostBatch, accCostsSep)
          }
        }
        .runSyncUnsafe(10.seconds)

      accCostBatch should contain theSameElementsAs accCostsSep
    }

  }

  "validateBlockCheckpoint" should "not return a checkpoint for an invalid block" in {
    val deploys          = Vector("@1!(1)").flatMap(mkTerm(_).toOption).map(ProtoUtil.termDeployNow)
    val processedDeploys = deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1, 1L)))
    val invalidHash      = ByteString.EMPTY
    val chain =
      createBlock[StateWithChain](Seq.empty, deploys = processedDeploys, tsHash = invalidHash)
        .runS(initState)
    val block = chain.idToBlocks(0)

    val Right(stateHash) = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay { validateBlockCheckpoint[Id](block, chain, runtimeManager) }
      }
      .runSyncUnsafe(10.seconds)

    stateHash should be(None)
  }

  it should "return a checkpoint with the right hash for a valid block" in {
    val deploys =
      Vector(
        "@1!(1)",
        "@2!(1)",
        "@2!(2)",
        "@2!(3)",
        "@2!(4)",
        "@2!(5)",
        "for (@x <- @1) { @2!(x) }",
        "for (@x <- @2) { @3!(x) }"
      ).flatMap(mkTerm(_).toOption)
        .map(ProtoUtil.termDeploy(_, System.currentTimeMillis(), accounting.MAX_VALUE))

    val (tsHash, computedTsHash) = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          val Right((preStateHash, computedTsHash, processedDeploys)) =
            computeDeploysCheckpoint[Id](Seq.empty, deploys, initState, runtimeManager)
          val chain: IndexedBlockDag =
            createBlock[StateWithChain](
              Seq.empty,
              deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
              tsHash = computedTsHash,
              preStateHash = preStateHash
            ).runS(initState)
          val block = chain.idToBlocks(0)

          val Right(tsHash) =
            validateBlockCheckpoint[Id](block, chain, runtimeManager)

          (tsHash, computedTsHash)
        }
      }
      .runSyncUnsafe(10.seconds)

    tsHash should be(Some(computedTsHash))
  }

  it should "pass linked list test" in {
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
    ).map(
      s =>
        ProtoUtil.termDeploy(
          InterpreterUtil.mkTerm(s).right.get,
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )
    )

    val (tsHash, computedTsHash) = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          val Right((preStateHash, computedTsHash, processedDeploys)) =
            computeDeploysCheckpoint[Id](Seq.empty, deploys, initState, runtimeManager)
          val chain: IndexedBlockDag =
            createBlock[StateWithChain](
              Seq.empty,
              deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
              tsHash = computedTsHash,
              preStateHash = preStateHash
            ).runS(initState)
          val block = chain.idToBlocks(0)

          val Right(tsHash) =
            validateBlockCheckpoint[Id](block, chain, runtimeManager)

          (tsHash, computedTsHash)
        }
      }
      .runSyncUnsafe(10.seconds)

    tsHash should be(Some(computedTsHash))
  }

  it should "pass persistent produce test with causality" in {
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
        .map(
          s =>
            ProtoUtil.termDeploy(
              InterpreterUtil.mkTerm(s).right.get,
              System.currentTimeMillis(),
              accounting.MAX_VALUE
            )
        )
    val (tsHash, computedTsHash) = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          val Right((preStateHash, computedTsHash, processedDeploys)) =
            computeDeploysCheckpoint[Id](Seq.empty, deploys, initState, runtimeManager)
          val chain: IndexedBlockDag =
            createBlock[StateWithChain](
              Seq.empty,
              deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
              tsHash = computedTsHash,
              preStateHash = preStateHash
            ).runS(initState)
          val block = chain.idToBlocks(0)

          val Right(tsHash) =
            validateBlockCheckpoint[Id](block, chain, runtimeManager)

          (tsHash, computedTsHash)
        }
      }
      .runSyncUnsafe(10.seconds)

    tsHash should be(Some(computedTsHash))
  }

  it should "pass tests involving primitives" in {
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
      ).map(
        s =>
          ProtoUtil.termDeploy(
            InterpreterUtil.mkTerm(s).right.get,
            System.currentTimeMillis(),
            accounting.MAX_VALUE
          )
      )
    val (tsHash, computedTsHash) = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          val Right((preStateHash, computedTsHash, processedDeploys)) =
            computeDeploysCheckpoint[Id](Seq.empty, deploys, initState, runtimeManager)
          val chain: IndexedBlockDag =
            createBlock[StateWithChain](
              Seq.empty,
              deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
              tsHash = computedTsHash,
              preStateHash = preStateHash
            ).runS(initState)
          val block = chain.idToBlocks(0)

          val Right(tsHash) =
            validateBlockCheckpoint[Id](block, chain, runtimeManager)
          (tsHash, computedTsHash)
        }
      }
      .runSyncUnsafe(10.seconds)

    tsHash should be(Some(computedTsHash))
  }

  it should "pass tests involving races" in {
    (0 to 10).foreach { _ =>
      val deploys =
        Vector(
          """
            | contract @"loop"(@xs) = {
            |   match xs {
            |     [] => {
            |       for (@winner <- @"ch") {
            |         @"return"!(winner)
            |       }
            |     }
            |     [first, ...rest] => {
            |       @"ch"!(first) | @"loop"!(rest)
            |     }
            |   }
            | } | @"loop"!(["a","b","c","d"])
            |""".stripMargin
        ).map(
          s =>
            ProtoUtil.termDeploy(
              InterpreterUtil.mkTerm(s).right.get,
              System.currentTimeMillis(),
              accounting.MAX_VALUE
            )
        )
      val (tsHash, computedTsHash) = mkRuntimeManager("interpreter-util-test")
        .use { runtimeManager =>
          Task.delay {
            val Right((preStateHash, computedTsHash, processedDeploys)) =
              computeDeploysCheckpoint[Id](
                Seq.empty,
                deploys,
                initState,
                runtimeManager
              )
            val chain: IndexedBlockDag =
              createBlock[StateWithChain](
                Seq.empty,
                deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                tsHash = computedTsHash,
                preStateHash = preStateHash
              ).runS(initState)
            val block = chain.idToBlocks(0)

            val Right(tsHash) =
              validateBlockCheckpoint[Id](block, chain, runtimeManager)
            (tsHash, computedTsHash)
          }
        }
        .runSyncUnsafe(10.seconds)

      tsHash should be(Some(computedTsHash))
    }
  }

  it should "return None for logs containing extra comm events" in {
    val deploys = (0 until 1).map(i => {
      val code = s"for(_ <- @$i){ Nil } | @$i!($i)"
      val term = InterpreterUtil.mkTerm(code).right.get
      ProtoUtil.termDeployNow(term)
    })

    val tsHash = mkRuntimeManager("interpreter-util-test")
      .use { runtimeManager =>
        Task.delay {
          val Right((preStateHash, computedTsHash, processedDeploys)) =
            computeDeploysCheckpoint[Id](Seq.empty, deploys, initState, runtimeManager)
          val intProcessedDeploys = processedDeploys.map(ProcessedDeployUtil.fromInternal)
          //create single deploy with log that includes excess comm events
          val badProcessedDeploy = intProcessedDeploys.head.copy(
            log = intProcessedDeploys.head.log ++ intProcessedDeploys.last.log
          )
          val chain: IndexedBlockDag =
            createBlock[StateWithChain](
              Seq.empty,
              deploys = Seq(badProcessedDeploy, intProcessedDeploys.last),
              tsHash = computedTsHash,
              preStateHash = preStateHash
            ).runS(initState)
          val block = chain.idToBlocks(0)

          val Right(tsHash) =
            validateBlockCheckpoint[Id](block, chain, runtimeManager)

          tsHash
        }
      }
      .runSyncUnsafe(10.seconds)

    tsHash should be(None)
  }

  it should "pass map update test" in {
    (0 to 10).foreach { _ =>
      val deploys =
        Vector(
          """
            | @"mapStore"!({}) |
            | contract @"store"(@value) = {
            |   new key in {
            |     for (@map <- @"mapStore") {
            |       @"mapStore"!(map.set(*key.toByteArray(), value))
            |     }
            |   }
            | }
            |""".stripMargin,
          """
            |@"store"!("1")
          """.stripMargin,
          """
            |@"store"!("2")
          """.stripMargin
        ).map(s => ProtoUtil.termDeployNow(InterpreterUtil.mkTerm(s).right.get))

      val (tsHash, computedTsHash) = mkRuntimeManager("interpreter-util-test")
        .use { runtimeManager =>
          Task.delay {
            val Right((preStateHash, computedTsHash, processedDeploys)) =
              computeDeploysCheckpoint[Id](
                Seq.empty,
                deploys,
                initState,
                runtimeManager
              )
            val chain: IndexedBlockDag =
              createBlock[StateWithChain](
                Seq.empty,
                deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                tsHash = computedTsHash,
                preStateHash = preStateHash
              ).runS(initState)
            val block = chain.idToBlocks(0)

            val Right(tsHash) =
              validateBlockCheckpoint[Id](block, chain, runtimeManager)

            (tsHash, computedTsHash)
          }
        }
        .runSyncUnsafe(10.seconds)

      tsHash should be(Some(computedTsHash))
    }
  }
}
