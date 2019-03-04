package coop.rchain.casper.util.rholang

import java.nio.file.Files

import cats.mtl.implicits._
import cats.{Id, Monad}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.blockstorage.{
  BlockDagRepresentation,
  BlockDagStorage,
  BlockStore,
  IndexedBlockDagStorage
}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil._
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.models.PCost
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.{StoreType, Time}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest._

import scala.concurrent.duration._

class InterpreterUtilTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {
  implicit val logEff                    = new LogStub[Task]
  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  val storageSize                        = 1024L * 1024
  val storageDirectory                   = Files.createTempDirectory("casper-interp-util-test")
  val activeRuntime =
    Runtime.create[Task, Task.Par](storageDirectory, storageSize, StoreType.LMDB).unsafeRunSync

  val runtimeManager = RuntimeManager.fromRuntime(activeRuntime).unsafeRunSync

  "computeBlockCheckpoint" should "compute the final post-state of a chain properly" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val genesisDeploys = Vector(
        "@1!(1)",
        "@2!(2)",
        "for(@a <- @1){ @123!(5 * a) }"
      ).map(ProtoUtil.sourceDeploy(_, System.currentTimeMillis(), accounting.MAX_VALUE))
      val genesisDeploysCost =
        genesisDeploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1)))

      val b1Deploys = Vector(
        "@1!(1)",
        "for(@a <- @2){ @456!(5 * a) }"
      ).map(ProtoUtil.sourceDeployNow)
      val b1DeploysCost = b1Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1L)))

      val b2Deploys = Vector(
        "for(@a <- @123; @b <- @456){ @1!(a + b) }"
      ).map(ProtoUtil.sourceDeployNow)
      val b2DeploysCost = b2Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1L)))

      val b3Deploys = Vector(
        "@7!(7)"
      ).map(ProtoUtil.sourceDeployNow)
      val b3DeploysCost = b3Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1L)))

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

      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          genesis                                     <- createBlock[Task](Seq.empty, deploys = genesisDeploysCost)
          b1                                          <- createBlock[Task](Seq(genesis.blockHash), deploys = b1DeploysCost)
          b2                                          <- createBlock[Task](Seq(b1.blockHash), deploys = b2DeploysCost)
          b3                                          <- createBlock[Task](Seq(b2.blockHash), deploys = b3DeploysCost)
          dag1                                        <- blockDagStorage.getRepresentation
          blockCheckpoint                             <- computeBlockCheckpoint(genesis, genesis, dag1, runtimeManager)
          (postGenStateHash, postGenProcessedDeploys) = blockCheckpoint
          _                                           <- injectPostStateHash[Task](0, genesis, postGenStateHash, postGenProcessedDeploys)
          genPostStateT                               <- runtimeManager.storageRepr(postGenStateHash)
          genPostState                                = genPostStateT.get

          _                                         = genPostState.contains("@{2}!(2)") should be(true)
          _                                         = genPostState.contains("@{123}!(5)") should be(true)
          dag2                                      <- blockDagStorage.getRepresentation
          blockCheckpointB1                         <- computeBlockCheckpoint(b1, genesis, dag2, runtimeManager)
          (postB1StateHash, postB1ProcessedDeploys) = blockCheckpointB1
          _                                         <- injectPostStateHash[Task](1, b1, postB1StateHash, postB1ProcessedDeploys)
          b1PostStateT                              <- runtimeManager.storageRepr(postB1StateHash)
          b1PostState                               = b1PostStateT.get
          _                                         = b1PostState.contains("@{1}!(1)") should be(true)
          _                                         = b1PostState.contains("@{123}!(5)") should be(true)
          _                                         = b1PostState.contains("@{456}!(10)") should be(true)
          dag3                                      <- blockDagStorage.getRepresentation
          blockCheckpointB2 <- computeBlockCheckpoint(
                                b2,
                                genesis,
                                dag3,
                                runtimeManager
                              )
          (postB2StateHash, postB2ProcessedDeploys) = blockCheckpointB2
          _                                         <- injectPostStateHash[Task](2, b2, postB2StateHash, postB2ProcessedDeploys)

          dag4 <- blockDagStorage.getRepresentation
          blockCheckpointB4 <- computeBlockCheckpoint(
                                b3,
                                genesis,
                                dag4,
                                runtimeManager
                              )
          (postb3StateHash, _) = blockCheckpointB4
          b3PostStateT         <- runtimeManager.storageRepr(postb3StateHash)
          b3PostState          = b3PostStateT.get

          _      = b3PostState.contains("@{1}!(1)") should be(true)
          _      = b3PostState.contains("@{1}!(15)") should be(true)
          result = b3PostState.contains("@{7}!(7)") should be(true)
        } yield result
      }
  }

  it should "merge histories in case of multiple parents" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val genesisDeploys = Vector(
        "@1!(1)",
        "@2!(2)",
        "for(@a <- @1){ @123!(5 * a) }"
      ).map(ProtoUtil.sourceDeployNow)
      val genesisDeploysWithCost =
        genesisDeploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1)))

      val b1Deploys = Vector(
        "@5!(5)",
        "for(@a <- @2){ @456!(5 * a) }"
      ).map(ProtoUtil.sourceDeployNow)
      val b1DeploysWithCost =
        b1Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(2L)))

      val b2Deploys = Vector(
        "@6!(6)"
      ).map(ProtoUtil.sourceDeployNow)
      val b2DeploysWithCost =
        b2Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1L)))

      val b3Deploys = Vector(
        "for(@a <- @123; @b <- @456){ @1!(a + b) }"
      ).map(ProtoUtil.sourceDeployNow)
      val b3DeploysWithCost =
        b3Deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(5L)))

      /*
       * DAG Looks like this:
       *
       *           b3
       *          /  \
       *        b1    b2
       *         \    /
       *         genesis
       */
      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          genesis                                     <- createBlock[Task](Seq.empty, deploys = genesisDeploysWithCost)
          b1                                          <- createBlock[Task](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
          b2                                          <- createBlock[Task](Seq(genesis.blockHash), deploys = b2DeploysWithCost)
          b3                                          <- createBlock[Task](Seq(b1.blockHash, b2.blockHash), deploys = b3DeploysWithCost)
          dag1                                        <- blockDagStorage.getRepresentation
          blockCheckpoint                             <- computeBlockCheckpoint(genesis, genesis, dag1, runtimeManager)
          (postGenStateHash, postGenProcessedDeploys) = blockCheckpoint
          _                                           <- injectPostStateHash[Task](0, genesis, postGenStateHash, postGenProcessedDeploys)
          dag2                                        <- blockDagStorage.getRepresentation
          blockCheckpointB1 <- computeBlockCheckpoint(
                                b1,
                                genesis,
                                dag2,
                                runtimeManager
                              )
          (postB1StateHash, postB1ProcessedDeploys) = blockCheckpointB1
          _                                         <- injectPostStateHash[Task](1, b1, postB1StateHash, postB1ProcessedDeploys)
          dag3                                      <- blockDagStorage.getRepresentation
          blockCheckpointB2 <- computeBlockCheckpoint(
                                b2,
                                genesis,
                                dag3,
                                runtimeManager
                              )
          (postB2StateHash, postB2ProcessedDeploys) = blockCheckpointB2
          _                                         <- injectPostStateHash[Task](2, b2, postB2StateHash, postB2ProcessedDeploys)
          updatedGenesis                            <- blockDagStorage.lookupByIdUnsafe(0)
          dag4                                      <- blockDagStorage.getRepresentation
          blockCheckpointB3 <- computeBlockCheckpoint(
                                b3,
                                updatedGenesis,
                                dag4,
                                runtimeManager
                              )
          (postb3StateHash, _) = blockCheckpointB3
          b3PostStateT         <- runtimeManager.storageRepr(postb3StateHash)
          b3PostState          = b3PostStateT.get

          _      = b3PostState.contains("@{1}!(15)") should be(true)
          _      = b3PostState.contains("@{5}!(5)") should be(true)
          result = b3PostState.contains("@{6}!(6)") should be(true)
        } yield result
      }
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
    val genesisDeploys = v.map(ProtoUtil.sourceDeployNow)
    genesisDeploys.map(d => ProcessedDeploy().withDeploy(d).withCost(c))
  }

  it should "merge histories in case of multiple parents with complex contract" ignore withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val contract = registry

      val genesisDeploysWithCost = prepareDeploys(Vector.empty, PCost(1))
      val b1DeploysWithCost      = prepareDeploys(Vector(contract), PCost(2L))
      val b2DeploysWithCost      = prepareDeploys(Vector(contract), PCost(1L))
      val b3DeploysWithCost      = prepareDeploys(Vector.empty, PCost(5L))

      /*
       * DAG Looks like this:
       *
       *           b3
       *          /  \
       *        b1    b2
       *         \    /
       *         genesis
       */

      mkRuntimeManager("interpreter-util-test")
        .use { runtimeManager =>
          def step(index: Int, genesis: BlockMessage) =
            for {
              b1  <- blockDagStorage.lookupByIdUnsafe(index)
              dag <- blockDagStorage.getRepresentation
              computeBlockCheckpointResult <- computeBlockCheckpoint(
                                               b1,
                                               genesis,
                                               dag,
                                               runtimeManager
                                             )
              (postB1StateHash, postB1ProcessedDeploys) = computeBlockCheckpointResult
              result <- injectPostStateHash[Task](
                         index,
                         b1,
                         postB1StateHash,
                         postB1ProcessedDeploys
                       )
            } yield result
          for {
            genesis   <- createBlock[Task](Seq.empty, deploys = genesisDeploysWithCost)
            b1        <- createBlock[Task](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
            b2        <- createBlock[Task](Seq(genesis.blockHash), deploys = b2DeploysWithCost)
            b3        <- createBlock[Task](Seq(b1.blockHash, b2.blockHash), deploys = b3DeploysWithCost)
            _         <- step(0, genesis)
            _         <- step(1, genesis)
            _         <- step(2, genesis)
            dag       <- blockDagStorage.getRepresentation
            postState <- validateBlockCheckpoint[Task](b3, dag, runtimeManager)
            result    = postState shouldBe Right(None)
          } yield result
        }
  }

  it should "merge histories in case of multiple parents (uneven histories)" ignore withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val contract = registry

      val genesisDeploysWithCost = prepareDeploys(Vector(contract), PCost(1))

      val b1DeploysWithCost = prepareDeploys(Vector(contract), PCost(2L))

      val b2DeploysWithCost = prepareDeploys(Vector(contract), PCost(1L))

      val b3DeploysWithCost = prepareDeploys(Vector(contract), PCost(5L))

      val b4DeploysWithCost = prepareDeploys(Vector(contract), PCost(5L))

      val b5DeploysWithCost = prepareDeploys(Vector(contract), PCost(5L))

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

      mkRuntimeManager("interpreter-util-test")
        .use { runtimeManager =>
          def step(index: Int, genesis: BlockMessage) =
            for {
              b1  <- blockDagStorage.lookupByIdUnsafe(index)
              dag <- blockDagStorage.getRepresentation
              computeBlockCheckpointResult <- computeBlockCheckpoint(
                                               b1,
                                               genesis,
                                               dag,
                                               runtimeManager
                                             )
              (postB1StateHash, postB1ProcessedDeploys) = computeBlockCheckpointResult
              result <- injectPostStateHash[Task](
                         index,
                         b1,
                         postB1StateHash,
                         postB1ProcessedDeploys
                       )
            } yield result
          for {
            genesis <- createBlock[Task](Seq.empty, deploys = genesisDeploysWithCost)
            b1      <- createBlock[Task](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
            b2      <- createBlock[Task](Seq(b1.blockHash), deploys = b2DeploysWithCost)
            b3      <- createBlock[Task](Seq(b1.blockHash), deploys = b3DeploysWithCost)
            b4      <- createBlock[Task](Seq(b3.blockHash), deploys = b4DeploysWithCost)
            b5      <- createBlock[Task](Seq(b2.blockHash, b4.blockHash), deploys = b5DeploysWithCost)
            dag1    <- blockDagStorage.getRepresentation
            computeBlockCheckpointResult <- computeBlockCheckpoint(
                                             genesis,
                                             genesis,
                                             dag1,
                                             runtimeManager
                                           )
            (postGenStateHash, postGenProcessedDeploys) = computeBlockCheckpointResult
            _                                           <- injectPostStateHash[Task](0, genesis, postGenStateHash, postGenProcessedDeploys)
            _                                           <- step(1, genesis)
            _                                           <- step(2, genesis)
            _                                           <- step(3, genesis)
            _                                           <- step(4, genesis)

            dag2      <- blockDagStorage.getRepresentation
            postState <- validateBlockCheckpoint[Task](b5, dag2, runtimeManager)
            result    = postState shouldBe Right(None)
          } yield result
        }
  }

  def computeSingleProcessedDeploy(
      runtimeManager: RuntimeManager[Task],
      dag: BlockDagRepresentation[Task],
      deploy: DeployData*
  )(implicit blockStore: BlockStore[Task]): Task[Seq[InternalProcessedDeploy]] =
    for {
      computeResult         <- computeDeploysCheckpoint[Task](Seq.empty, deploy, dag, runtimeManager)
      Right((_, _, result)) = computeResult
    } yield result

  "computeDeploysCheckpoint" should "aggregate cost of deploying rholang programs within the block" in withStorage {
    implicit blockStore =>
      implicit blockDagStorage =>
        //reference costs
        //deploy each Rholang program separately and record its cost
        val deploy1 = ProtoUtil.sourceDeploy(
          "@1!(Nil)",
          System.currentTimeMillis(),
          accounting.MAX_VALUE
        )
        val deploy2 =
          ProtoUtil.sourceDeploy(
            "@3!([1,2,3,4])",
            System.currentTimeMillis(),
            accounting.MAX_VALUE
          )
        val deploy3 =
          ProtoUtil.sourceDeploy(
            "for(@x <- @0) { @4!(x.toByteArray()) }",
            System.currentTimeMillis(),
            accounting.MAX_VALUE
          )
        mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
          for {
            dag          <- blockDagStorage.getRepresentation
            cost1        <- computeSingleProcessedDeploy(runtimeManager, dag, deploy1)
            cost2        <- computeSingleProcessedDeploy(runtimeManager, dag, deploy2)
            cost3        <- computeSingleProcessedDeploy(runtimeManager, dag, deploy3)
            accCostsSep  = cost1 ++ cost2 ++ cost3
            singleDeploy = Seq(deploy1, deploy2, deploy3)
            accCostBatch <- computeSingleProcessedDeploy(runtimeManager, dag, singleDeploy: _*)
          } yield accCostBatch should contain theSameElementsAs accCostsSep
      }
  }

  it should "return cost of deploying even if one of the programs withing the deployment throws an error" in
    pendingUntilFixed { //reference costs
      withStorage { implicit blockStore => implicit blockDagStorage =>
        //deploy each Rholang program separately and record its cost
        val deploy1 =
          ProtoUtil.sourceDeploy(
            "@1!(Nil)",
            System.currentTimeMillis(),
            accounting.MAX_VALUE
          )
        val deploy2 =
          ProtoUtil.sourceDeploy(
            "@2!([1,2,3,4])",
            System.currentTimeMillis(),
            accounting.MAX_VALUE
          )
        mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
          for {
            dag <- blockDagStorage.getRepresentation

            cost1 <- computeSingleProcessedDeploy(runtimeManager, dag, deploy1)
            cost2 <- computeSingleProcessedDeploy(runtimeManager, dag, deploy2)

            accCostsSep = cost1 ++ cost2

            deployErr = ProtoUtil.sourceDeploy(
              "@3!(\"a\" + 3)",
              System.currentTimeMillis(),
              accounting.MAX_VALUE
            )
            batchDeploy  = Seq(deploy1, deploy2, deployErr)
            accCostBatch <- computeSingleProcessedDeploy(runtimeManager, dag, batchDeploy: _*)
          } yield accCostBatch should contain theSameElementsAs accCostsSep
      }
      }
    }

  "validateBlockCheckpoint" should "not return a checkpoint for an invalid block" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val deploys = Vector("@1!(1)").map(ProtoUtil.sourceDeployNow)
      val processedDeploys =
        deploys.map(d => ProcessedDeploy().withDeploy(d).withCost(PCost(1L)))
      val invalidHash = ByteString.EMPTY
      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          block            <- createBlock[Task](Seq.empty, deploys = processedDeploys, tsHash = invalidHash)
          dag              <- blockDagStorage.getRepresentation
          validateResult   <- validateBlockCheckpoint[Task](block, dag, runtimeManager)
          Right(stateHash) = validateResult
        } yield stateHash should be(None)
      }
  }

  it should "return a checkpoint with the right hash for a valid block" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
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
        ).map(ProtoUtil.sourceDeploy(_, System.currentTimeMillis(), accounting.MAX_VALUE))
      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          dag1 <- blockDagStorage.getRepresentation
          deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                Seq.empty,
                                deploys,
                                dag1,
                                runtimeManager
                              )
          Right((preStateHash, computedTsHash, processedDeploys)) = deploysCheckpoint
          block <- createBlock[Task](
                    Seq.empty,
                    deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                    tsHash = computedTsHash,
                    preStateHash = preStateHash
                  )
          dag2 <- blockDagStorage.getRepresentation

          validateResult <- validateBlockCheckpoint[Task](block, dag2, runtimeManager)
          Right(tsHash)  = validateResult
        } yield tsHash should be(Some(computedTsHash))
      }
  }

  it should "pass linked list test" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
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
          ProtoUtil.sourceDeploy(
            s,
            System.currentTimeMillis(),
            accounting.MAX_VALUE
          )
      )

      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          dag1 <- blockDagStorage.getRepresentation
          deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                Seq.empty,
                                deploys,
                                dag1,
                                runtimeManager
                              )
          Right((preStateHash, computedTsHash, processedDeploys)) = deploysCheckpoint
          block <- createBlock[Task](
                    Seq.empty,
                    deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                    tsHash = computedTsHash,
                    preStateHash = preStateHash
                  )
          dag2           <- blockDagStorage.getRepresentation
          validateResult <- validateBlockCheckpoint[Task](block, dag2, runtimeManager)
          Right(tsHash)  = validateResult
        } yield tsHash should be(Some(computedTsHash))
      }
  }

  it should "pass persistent produce test with causality" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
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
              ProtoUtil.sourceDeploy(
                s,
                System.currentTimeMillis(),
                accounting.MAX_VALUE
              )
          )
      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          dag1 <- blockDagStorage.getRepresentation
          deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                Seq.empty,
                                deploys,
                                dag1,
                                runtimeManager
                              )
          Right((preStateHash, computedTsHash, processedDeploys)) = deploysCheckpoint
          block <- createBlock[Task](
                    Seq.empty,
                    deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                    tsHash = computedTsHash,
                    preStateHash = preStateHash
                  )
          dag2           <- blockDagStorage.getRepresentation
          validateResult <- validateBlockCheckpoint[Task](block, dag2, runtimeManager)
          Right(tsHash)  = validateResult
        } yield tsHash should be(Some(computedTsHash))
      }
  }

  it should "pass tests involving primitives" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
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
            ProtoUtil.sourceDeploy(
              s,
              System.currentTimeMillis(),
              accounting.MAX_VALUE
            )
        )
      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          dag1 <- blockDagStorage.getRepresentation
          deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                Seq.empty,
                                deploys,
                                dag1,
                                runtimeManager
                              )
          Right((preStateHash, computedTsHash, processedDeploys)) = deploysCheckpoint
          block <- createBlock[Task](
                    Seq.empty,
                    deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                    tsHash = computedTsHash,
                    preStateHash = preStateHash
                  )
          dag2           <- blockDagStorage.getRepresentation
          validateResult <- validateBlockCheckpoint[Task](block, dag2, runtimeManager)
          Right(tsHash)  = validateResult
        } yield tsHash should be(Some(computedTsHash))
      }
  }

  it should "pass tests involving races" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      (0 to 10).toList.traverse_ { _ =>
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
              ProtoUtil.sourceDeploy(
                s,
                System.currentTimeMillis(),
                accounting.MAX_VALUE
              )
          )

        mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
          for {
            dag1 <- blockDagStorage.getRepresentation
            deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                  Seq.empty,
                                  deploys,
                                  dag1,
                                  runtimeManager
                                )
            Right((preStateHash, computedTsHash, processedDeploys)) = deploysCheckpoint
            block <- createBlock[Task](
                      Seq.empty,
                      deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                      tsHash = computedTsHash,
                      preStateHash = preStateHash
                    )
            dag2 <- blockDagStorage.getRepresentation

            validateResult <- validateBlockCheckpoint[Task](block, dag2, runtimeManager)
            Right(tsHash)  = validateResult
          } yield tsHash should be(Some(computedTsHash))
        }
      }
  }

  it should "return None for logs containing extra comm events" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val deploys = (0 until 1).map(i => {
        val code = s"for(_ <- @$i){ Nil } | @$i!($i)"
        ProtoUtil.sourceDeployNow(code)
      })

      mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
        for {
          dag1 <- blockDagStorage.getRepresentation
          deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                Seq.empty,
                                deploys,
                                dag1,
                                runtimeManager
                              )
          Right((preStateHash, computedTsHash, processedDeploys)) = deploysCheckpoint
          intProcessedDeploys                                     = processedDeploys.map(ProcessedDeployUtil.fromInternal)
          //create single deploy with log that includes excess comm events
          badProcessedDeploy = intProcessedDeploys.head.copy(
            log = intProcessedDeploys.head.log ++ intProcessedDeploys.last.log
          )
          block <- createBlock[Task](
                    Seq.empty,
                    deploys = Seq(badProcessedDeploy, intProcessedDeploys.last),
                    tsHash = computedTsHash,
                    preStateHash = preStateHash
                  )
          dag2           <- blockDagStorage.getRepresentation
          validateResult <- validateBlockCheckpoint[Task](block, dag2, runtimeManager)
          Right(tsHash)  = validateResult
        } yield tsHash should be(None)
      }
  }

  it should "pass map update test" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      (0 to 10).toList.traverse_ { _ =>
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
          ).map(s => ProtoUtil.sourceDeployNow(s))

        mkRuntimeManager("interpreter-util-test").use { runtimeManager =>
          for {
            dag1 <- blockDagStorage.getRepresentation
            deploysCheckpoint <- computeDeploysCheckpoint[Task](
                                  Seq.empty,
                                  deploys,
                                  dag1,
                                  runtimeManager
                                )
            Right((preStateHash, computedTsHash, processedDeploys)) = deploysCheckpoint
            block <- createBlock[Task](
                      Seq.empty,
                      deploys = processedDeploys.map(ProcessedDeployUtil.fromInternal),
                      tsHash = computedTsHash,
                      preStateHash = preStateHash
                    )
            dag2           <- blockDagStorage.getRepresentation
            validateResult <- validateBlockCheckpoint[Task](block, dag2, runtimeManager)
            Right(tsHash)  = validateResult
          } yield tsHash should be(Some(computedTsHash))
        }
      }
  }

  "findMultiParentsBlockHashesForReplay" should "filter out duplicate ancestors of main parent block" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val genesisDeploysWithCost = prepareDeploys(Vector.empty, PCost(1))
      val b1DeploysWithCost      = prepareDeploys(Vector("@1!(1)"), PCost(1))
      val b2DeploysWithCost      = prepareDeploys(Vector("@2!(2)"), PCost(1))
      val b3DeploysWithCost      = prepareDeploys(Vector("@3!(3)"), PCost(1))

      /*
       * DAG Looks like this:
       *
       *           b3
       *          /  \
       *        b1    b2
       *         \    /
       *         genesis
       */

      mkRuntimeManager("interpreter-util-test")
        .use { runtimeManager =>
          def step(index: Int, genesis: BlockMessage) =
            for {
              b1  <- blockDagStorage.lookupByIdUnsafe(index)
              dag <- blockDagStorage.getRepresentation
              computeBlockCheckpointResult <- computeBlockCheckpoint(
                                               b1,
                                               genesis,
                                               dag,
                                               runtimeManager
                                             )
              (postB1StateHash, postB1ProcessedDeploys) = computeBlockCheckpointResult
              result <- injectPostStateHash[Task](
                         index,
                         b1,
                         postB1StateHash,
                         postB1ProcessedDeploys
                       )
            } yield result
          for {
            genesis <- createBlock[Task](Seq.empty, deploys = genesisDeploysWithCost)
            b1      <- createBlock[Task](Seq(genesis.blockHash), deploys = b1DeploysWithCost)
            b2      <- createBlock[Task](Seq(genesis.blockHash), deploys = b2DeploysWithCost)
            b3      <- createBlock[Task](Seq(b1.blockHash, b2.blockHash), deploys = b3DeploysWithCost)
            _       <- step(0, genesis)
            _       <- step(1, genesis)
            _       <- step(2, genesis)
            dag     <- blockDagStorage.getRepresentation
            blockHashes <- InterpreterUtil.findMultiParentsBlockHashesForReplay(
                            Seq(b1, b2),
                            dag
                          )
            _ = withClue("Main parent hasn't been filtered out: ") { blockHashes.size shouldBe (1) }

          } yield ()
        }
  }

}
