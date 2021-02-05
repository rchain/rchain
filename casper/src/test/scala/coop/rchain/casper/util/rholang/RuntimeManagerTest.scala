package coop.rchain.casper.util.rholang

import cats.effect.Resource
import cats.syntax.all._
import cats.{Functor, Id}
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.ProcessedSystemDeploy.Failed
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.syntax._
import coop.rchain.rspace.syntax._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.SystemDeployPlayResult.{PlayFailed, PlaySucceeded}
import coop.rchain.casper.util.rholang.SystemDeployReplayResult.{ReplayFailed, ReplaySucceeded}
import coop.rchain.casper.util.rholang.costacc._
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.PCost
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.{accounting, ParBuilderUtil, RhoRuntime}
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import coop.rchain.{metrics, rholang}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class RuntimeManagerTest extends FlatSpec with Matchers {

  implicit val timeF: Time[Task]         = new LogicalTime[Task]
  implicit val log: Log[Task]            = Log.log[Task]
  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

  val genesisContext = GenesisBuilder.buildGenesis()
  val genesis        = genesisContext.genesisBlock

  val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
    Resources
      .copyStorage[Task](genesisContext.storageDirectory)
      .map(_.storageDir)
      .evalMap(Resources.mkTestRNodeStoreManager[Task])
      .evalMap(Resources.mkRuntimeManagerAt[Task])

  val runtimeAndManager: Resource[Task, (interpreter.RhoRuntime[Task], RuntimeManager[Task])] =
    for {
      dirs                         <- Resources.copyStorage[Task](genesisContext.storageDirectory)
      kvm                          <- Resource.liftF(Resources.mkTestRNodeStoreManager[Task](dirs.storageDir))
      rspaceStore                  <- Resource.liftF(kvm.rSpaceStores)
      runtimes                     <- Resource.liftF(RhoRuntime.createRuntimes[Task](rspaceStore))
      (runtime, replayRuntime, hr) = runtimes
      rm                           <- Resource.liftF(RuntimeManager.fromRuntimes[Task](runtime, replayRuntime, hr))
    } yield (runtime, rm)

  private def computeState[F[_]: Functor](
      runtimeManager: RuntimeManager[F],
      deploy: Signed[DeployData],
      stateHash: StateHash
  ): F[(StateHash, ProcessedDeploy)] =
    for {
      res <- runtimeManager.computeState(stateHash)(
              deploy :: Nil,
              Nil,
              BlockData(deploy.data.timestamp, 0, genesisContext.validatorPks.head, 0),
              Map.empty[BlockHash, Validator]
            )
      (hash, Seq(result), _) = res
    } yield (hash, result)

  private def replayComputeState[F[_]: Functor](runtimeManager: RuntimeManager[F])(
      stateHash: StateHash,
      processedDeploy: ProcessedDeploy
  ): F[Either[ReplayFailure, StateHash]] =
    runtimeManager.replayComputeState(stateHash)(
      processedDeploy :: Nil,
      Nil,
      BlockData(
        processedDeploy.deploy.data.timestamp,
        0,
        genesisContext.validatorPks.head,
        0
      ),
      Map.empty[BlockHash, Validator],
      isGenesis = false
    )

  "computeState" should "charge for deploys" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      val genPostState = genesis.body.state.postStateHash
      val source       = """
                            # new rl(`rho:registry:lookup`), listOpsCh in {
                            #   rl!(`rho:lang:listOps`, *listOpsCh) |
                            #   for(x <- listOpsCh){
                            #     Nil
                            #   }
                            # }
                            #""".stripMargin('#')
      // TODO: Prohibit negative gas prices and gas limits in deploys.
      // TODO: Make minimum maximum yield for deploy parameter of node.
      ConstructDeploy.sourceDeployNowF(source = source, phloLimit = 100000) >>= { deploy =>
        computeState(runtimeManager, deploy, genPostState) >>= {
          case (playStateHash1, processedDeploy) =>
            replayComputeState(runtimeManager)(genPostState, processedDeploy) map {
              case Right(replayStateHash1) =>
                assert(playStateHash1 != genPostState && replayStateHash1 == playStateHash1)
              case Left(replayFailure) => fail(s"Unexpected replay failure: $replayFailure")
            }
        }
      }
    }
  }

  private def compareSuccessfulSystemDeploys[S <: SystemDeploy](
      runtimeManager: RuntimeManager[Task]
  )(startState: StateHash)(
      playSystemDeploy: S,
      replaySystemDeploy: S
  )(resultAssertion: S#Result => Boolean): Task[StateHash] =
    runtimeManager.withRuntime(
      runtime =>
        for {
          _ <- runtime.setBlockData(BlockData(0, 0, genesisContext.validatorPks.head, 0))
          r <- runtime.playSystemDeploy(startState)(playSystemDeploy).attempt >>= {
                case Right(PlaySucceeded(finalPlayStateHash, processedSystemDeploy, playResult)) =>
                  assert(resultAssertion(playResult))
                  runtimeManager.withReplayRuntime(
                    runtime =>
                      for {
                        _ <- runtime.setBlockData(
                              BlockData(0, 0, genesisContext.validatorPks.head, 0)
                            )
                        r <- runtime
                              .replaySystemDeploy(startState)(
                                replaySystemDeploy,
                                processedSystemDeploy
                              )
                              .attempt
                              .map {
                                case Right(Right(systemDeployReplayResult)) =>
                                  systemDeployReplayResult match {
                                    case ReplaySucceeded(finalReplayStateHash, replayResult) =>
                                      assert(finalPlayStateHash == finalReplayStateHash)
                                      assert(playResult == replayResult)
                                      finalReplayStateHash
                                    case ReplayFailed(systemDeployError) =>
                                      fail(
                                        s"Unexpected user error during replay: ${systemDeployError.errorMessage}"
                                      )
                                  }
                                case Right(Left(replayFailure)) =>
                                  fail(s"Unexpected replay failure: $replayFailure")
                                case Left(throwable) =>
                                  fail(
                                    s"Unexpected system error during replay: ${throwable.getMessage}"
                                  )
                              }
                      } yield r
                  )

                case Right(PlayFailed(Failed(_, errorMsg))) =>
                  fail(s"Unexpected user error during play: $errorMsg")
                case Left(throwable) =>
                  fail(s"Unexpected system error during play: ${throwable.getMessage}")
              }
        } yield r
    )

  "PreChargeDeploy" should "reduce user account balance by the correct amount" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      val userPk = ConstructDeploy.defaultPub
      compareSuccessfulSystemDeploys(runtimeManager)(genesis.body.state.postStateHash)(
        new PreChargeDeploy(
          chargeAmount = 9000000,
          pk = userPk,
          rand = Blake2b512Random(Array(0.toByte))
        ),
        new PreChargeDeploy(
          chargeAmount = 9000000,
          pk = userPk,
          rand = Blake2b512Random(Array(0.toByte))
        )
      )(_ => true) >>= { stateHash0 =>
        compareSuccessfulSystemDeploys(runtimeManager)(stateHash0)(
          new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(1.toByte))),
          new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(1.toByte)))
        )(_ == 0) >>= { stateHash1 =>
          compareSuccessfulSystemDeploys(runtimeManager)(stateHash1)(
            new RefundDeploy(refundAmount = 9000000, Blake2b512Random(Array(2.toByte))),
            new RefundDeploy(refundAmount = 9000000, Blake2b512Random(Array(2.toByte)))
          )(_ => true) >>= { stateHash2 =>
            compareSuccessfulSystemDeploys(runtimeManager)(stateHash2)(
              new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(3.toByte))),
              new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(3.toByte)))
            )(_ == 9000000)
          }
        }
      }
    }
  }

  "closeBlock" should "make epoch change and reward validator" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      compareSuccessfulSystemDeploys(runtimeManager)(genesis.body.state.postStateHash)(
        new CloseBlockDeploy(
          initialRand = Blake2b512Random(Array(0.toByte))
        ),
        new CloseBlockDeploy(
          initialRand = Blake2b512Random(Array(0.toByte))
        )
      )(_ => true)
    }
  }

  "closeBlock replay" should "fail with different random seed" in {
    an[Exception] should be thrownBy effectTest({
      runtimeManagerResource.use { runtimeManager =>
        compareSuccessfulSystemDeploys(runtimeManager)(genesis.body.state.postStateHash)(
          new CloseBlockDeploy(
            initialRand = Blake2b512Random(Array(0.toByte))
          ),
          new CloseBlockDeploy(
            initialRand = Blake2b512Random(Array(1.toByte))
          )
        )(_ => true)
      }
    })
  }

  "BalanceDeploy" should "compute REV balances" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      val userPk = ConstructDeploy.defaultPub
      compareSuccessfulSystemDeploys(runtimeManager)(genesis.body.state.postStateHash)(
        new CheckBalance(pk = userPk, rand = Blake2b512Random(Array.empty[Byte])),
        new CheckBalance(pk = userPk, rand = Blake2b512Random(Array.empty[Byte]))
      )(_ == 9000000)
    }
  }

  "computeState" should "capture rholang errors" in effectTest {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) } | @"x"!(1) | @"y"!("hi") """
    for {
      deploy <- ConstructDeploy.sourceDeployNowF(badRholang)
      result <- runtimeManagerResource.use(
                 computeState(_, deploy, genesis.body.state.postStateHash)
               )
      _ = result._2.isFailed should be(true)
    } yield ()
  }

  "computeState then computeBonds" should "be replayable after-all" in effectTest {

    import cats.instances.vector._

    val gps = genesis.body.state.postStateHash

    val s0 = "@1!(1)"
    val s1 = "@2!(2)"
    val s2 = "for(@a <- @1){ @123!(5 * a) }"

    val deploys0F = Vector(s0, s1, s2).traverse(ConstructDeploy.sourceDeployNowF(_))

    val s3 = "@1!(1)"
    val s4 = "for(@a <- @2){ @456!(5 * a) }"

    val deploys1F = Vector(s3, s4).traverse(ConstructDeploy.sourceDeployNowF(_))

    runtimeManagerResource.use { runtimeManager =>
      for {
        deploys0 <- deploys0F
        deploys1 <- deploys1F
        time     <- timeF.currentMillis
        playStateHash0AndProcessedDeploys0 <- runtimeManager.computeState(gps)(
                                               deploys0.toList,
                                               CloseBlockDeploy(
                                                 SystemDeployUtil
                                                   .generateCloseDeployRandomSeed(
                                                     genesisContext.validatorPks.head,
                                                     0
                                                   )
                                               ) :: Nil,
                                               BlockData(
                                                 time,
                                                 0L,
                                                 genesisContext.validatorPks.head,
                                                 0
                                               ),
                                               Map.empty
                                             )
        (playStateHash0, processedDeploys0, processedSysDeploys0) = playStateHash0AndProcessedDeploys0
        bonds0                                                    <- runtimeManager.computeBonds(playStateHash0)
        replayError0OrReplayStateHash0 <- runtimeManager.replayComputeState(gps)(
                                           processedDeploys0,
                                           processedSysDeploys0,
                                           BlockData(
                                             time,
                                             0L,
                                             genesisContext.validatorPks.head,
                                             0
                                           ),
                                           Map.empty,
                                           isGenesis = false
                                         )
        Right(replayStateHash0) = replayError0OrReplayStateHash0
        _                       = assert(playStateHash0 == replayStateHash0)
        bonds1                  <- runtimeManager.computeBonds(playStateHash0)
        _                       = assert(bonds0 == bonds1)
        playStateHash1AndProcessedDeploys1 <- runtimeManager.computeState(playStateHash0)(
                                               deploys1.toList,
                                               CloseBlockDeploy(
                                                 SystemDeployUtil
                                                   .generateCloseDeployRandomSeed(
                                                     genesisContext.validatorPks.head,
                                                     0
                                                   )
                                               ) :: Nil,
                                               BlockData(
                                                 time,
                                                 0L,
                                                 genesisContext.validatorPks.head,
                                                 0
                                               ),
                                               Map.empty
                                             )
        (playStateHash1, processedDeploys1, processedSysDeploys1) = playStateHash1AndProcessedDeploys1
        bonds2                                                    <- runtimeManager.computeBonds(playStateHash1)
        replayError1OrReplayStateHash1 <- runtimeManager.replayComputeState(playStateHash0)(
                                           processedDeploys1,
                                           processedSysDeploys1,
                                           BlockData(
                                             time,
                                             0L,
                                             genesisContext.validatorPks.head,
                                             0
                                           ),
                                           Map.empty,
                                           isGenesis = false
                                         )
        Right(replayStateHash1) = replayError1OrReplayStateHash1
        _                       = assert(playStateHash1 == replayStateHash1)
        bonds3                  <- runtimeManager.computeBonds(playStateHash1)
        _                       = assert(bonds2 == bonds3)
      } yield ()
    }
  }

  it should "capture rholang parsing errors and charge for parsing" in effectTest {
    val badRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!("hi") """
    for {
      deploy <- ConstructDeploy.sourceDeployNowF(badRholang)
      result <- runtimeManagerResource.use(
                 computeState(_, deploy, genesis.body.state.postStateHash)
               )
      _ = result._2.isFailed should be(true)
      _ = result._2.cost.cost shouldEqual accounting.parsingCost(badRholang).value
    } yield ()
  }

  it should "charge for parsing and execution" in effectTest {
    val correctRholang = """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!(2) }"""

    runtimeAndManager
      .use {
        case (runtime, runtimeManager) => {
          implicit val rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
          val initialPhlo                     = Cost.UNSAFE_MAX
          for {
            deploy <- ConstructDeploy.sourceDeployNowF(correctRholang)

            _             <- runtime.cost.set(initialPhlo)
            term          <- ParBuilderUtil.buildNormalizedTerm[Task](deploy.data.term)
            _             <- runtime.inj(term)
            phlosLeft     <- runtime.cost.get
            reductionCost = initialPhlo - phlosLeft

            parsingCost = accounting.parsingCost(correctRholang)

            result <- computeState(runtimeManager, deploy, genesis.body.state.postStateHash)

            _ = result._2.cost.cost shouldEqual (reductionCost + parsingCost).value
          } yield ()
        }
      }
  }

  "captureResult" should "return the value at the specified channel after a rholang computation" in effectTest {
    val purseValue = "37"

    runtimeManagerResource.use { mgr =>
      for {
        deploy0 <- ConstructDeploy.sourceDeployNowF(
                    s"""
                      |new rl(`rho:registry:lookup`), NonNegativeNumberCh in {
                      |  rl!(`rho:lang:nonNegativeNumber`, *NonNegativeNumberCh) |
                      |  for(@(_, NonNegativeNumber) <- NonNegativeNumberCh) {
                      |    @NonNegativeNumber!($purseValue, "nn")
                      |  }
                      |}""".stripMargin
                  )
        result0 <- computeState(mgr, deploy0, genesis.body.state.postStateHash)
        hash    = result0._1
        deploy1 <- ConstructDeploy.sourceDeployNowF(
                    s"""new return in { for(nn <- @"nn"){ nn!("value", *return) } } """
                  )
        result1 <- mgr.captureResults(hash, deploy1)

        _ = result1.size should be(1)
        _ = result1.head should be(ParBuilderUtil.mkTerm(purseValue).right.get)
      } yield ()
    }
  }

  it should "handle multiple results and no results appropriately" in {
    val n           = 8
    val returns     = (1 to n).map(i => s""" return!($i) """).mkString("|")
    val term        = s""" new return in { $returns } """
    val termNoRes   = s""" new x, return in { $returns } """
    val deploy      = ConstructDeploy.sourceDeploy(term, timestamp = 0)
    val deployNoRes = ConstructDeploy.sourceDeploy(termNoRes, timestamp = 0)
    val manyResults =
      runtimeManagerResource
        .use(
          mgr =>
            for {
              hash <- RuntimeManager.emptyStateHashFixed.pure[Task]
              res  <- mgr.captureResults(hash, deploy)
            } yield res
        )
        .runSyncUnsafe(10.seconds)
    val noResults =
      runtimeManagerResource
        .use(
          mgr =>
            for {
              hash <- RuntimeManager.emptyStateHashFixed.pure[Task]
              res  <- mgr.captureResults(hash, deployNoRes)
            } yield res
        )
        .runSyncUnsafe(10.seconds)

    noResults.isEmpty should be(true)

    manyResults.size should be(n)
    (1 to n).forall(i => manyResults.contains(ParBuilderUtil.mkTerm(i.toString).right.get)) should be(
      true
    )
  }

  "captureResult" should "throw error if execution fails" in {
    val term   = s""" new return in { return.undefined() } """
    val deploy = ConstructDeploy.sourceDeploy(term, timestamp = 0)
    val task =
      runtimeManagerResource
        .use(
          mgr =>
            for {
              hash <- RuntimeManager.emptyStateHashFixed.pure[Task]
              res  <- mgr.captureResults(hash, deploy)
            } yield res
        )

    Await.result(task.failed.runToFuture, 1.seconds) shouldBe a[BugFoundError]
  }

  "emptyStateHash" should "not remember previous hot store state" in effectTest {
    implicit val timeEff: LogicalTime[Id] = new LogicalTime[Id]

    val term = ConstructDeploy.basicDeployData[Id](0)

    def run: Task[StateHash] =
      runtimeManagerResource
        .use { m =>
          for {
            hash <- RuntimeManager.emptyStateHashFixed.pure[Task]
            afterHash <- computeState(m, term, genesis.body.state.postStateHash)
                          .map(_ => hash)
          } yield afterHash
        }

    for {
      res            <- run.product(run)
      (hash1, hash2) = res
      _              = hash1 should be(hash2)
    } yield ()
  }

  "computeState" should "be replayed by replayComputeState" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      for {
        deploy <- ConstructDeploy.sourceDeployNowF(
                   """
                                                            # new deployerId(`rho:rchain:deployerId`),
                                                            #     rl(`rho:registry:lookup`),
                                                            #     revAddressOps(`rho:rev:address`),
                                                            #     revAddressCh,
                                                            #     revVaultCh in {
                                                            #   rl!(`rho:rchain:revVault`, *revVaultCh) |
                                                            #   revAddressOps!("fromDeployerId", *deployerId, *revAddressCh) |
                                                            #   for(@userRevAddress <- revAddressCh; @(_, revVault) <- revVaultCh){
                                                            #     new userVaultCh in {
                                                            #       @revVault!("findOrCreate", userRevAddress, *userVaultCh) |
                                                            #       for(@(true, userVault) <- userVaultCh){
                                                            #         @userVault!("balance", "IGNORE")
                                                            #       }
                                                            #     }
                                                            #   }
                                                            # }
                                                            #""".stripMargin('#')
                 )
        time          <- timeF.currentMillis
        genPostState  = genesis.body.state.postStateHash
        blockData     = BlockData(time, 0L, genesisContext.validatorPks.head, 0)
        invalidBlocks = Map.empty[BlockHash, Validator]
        computeStateResult <- runtimeManager.computeState(genPostState)(
                               deploy :: Nil,
                               CloseBlockDeploy(
                                 SystemDeployUtil.generateCloseDeployRandomSeed(
                                   blockData.sender,
                                   blockData.seqNum
                                 )
                               ) :: Nil,
                               blockData,
                               invalidBlocks
                             )
        (playPostState, processedDeploys, processedSystemDeploys) = computeStateResult
        replayComputeStateResult <- runtimeManager.replayComputeState(genPostState)(
                                     processedDeploys,
                                     processedSystemDeploys,
                                     blockData,
                                     invalidBlocks,
                                     isGenesis = false
                                   )
      } yield {
        replayComputeStateResult match {
          case Right(replayPostState) =>
            assert(playPostState == replayPostState)
          case Left(replayFailure) => fail(s"Found replay failure: ${replayFailure}")
        }
      }
    }
  }

  "computeState" should "charge deploys separately" in effectTest {

    def deployCost(p: Seq[ProcessedDeploy]): Long = p.map(_.cost.cost).sum

    runtimeManagerResource.use { mgr =>
      for {
        deploy0 <- ConstructDeploy.sourceDeployNowF(""" for(@x <- @"w") { @"z"!("Got x") } """)
        deploy1 <- ConstructDeploy.sourceDeployNowF(
                    """ for(@x <- @"x"; @y <- @"y"){ @"xy"!(x + y) | @"x"!(1) | @"y"!(10) } """
                  )
        time          <- timeF.currentMillis
        genPostState  = genesis.body.state.postStateHash
        blockData     = BlockData(time, 0L, genesisContext.validatorPks.head, 0)
        invalidBlocks = Map.empty[BlockHash, Validator]
        firstDeploy <- mgr
                        .computeState(genPostState)(
                          deploy0 :: Nil,
                          CloseBlockDeploy(
                            SystemDeployUtil
                              .generateCloseDeployRandomSeed(blockData.sender, blockData.seqNum)
                          ) :: Nil,
                          blockData,
                          invalidBlocks
                        )
                        .map(_._2)
        secondDeploy <- mgr
                         .computeState(genPostState)(
                           deploy1 :: Nil,
                           CloseBlockDeploy(
                             SystemDeployUtil
                               .generateCloseDeployRandomSeed(blockData.sender, blockData.seqNum)
                           ) :: Nil,
                           blockData,
                           invalidBlocks
                         )
                         .map(_._2)
        compoundDeploy <- mgr
                           .computeState(genPostState)(
                             deploy0 :: deploy1 :: Nil,
                             CloseBlockDeploy(
                               SystemDeployUtil
                                 .generateCloseDeployRandomSeed(blockData.sender, blockData.seqNum)
                             ) :: Nil,
                             blockData,
                             invalidBlocks
                           )
                           .map(_._2)
        _                  = assert(firstDeploy.size == 1)
        _                  = assert(secondDeploy.size == 1)
        _                  = assert(compoundDeploy.size == 2)
        firstDeployCost    = deployCost(firstDeploy)
        secondDeployCost   = deployCost(secondDeploy)
        compoundDeployCost = deployCost(compoundDeploy)
        _                  = assert(firstDeployCost < compoundDeployCost)
        _                  = assert(secondDeployCost < compoundDeployCost)
        _ = assert(
          firstDeployCost == deployCost(
            compoundDeploy.find(_.deploy == firstDeploy.head.deploy).toVector
          )
        )
        _ = assert(
          secondDeployCost == deployCost(
            compoundDeploy.find(_.deploy == secondDeploy.head.deploy).toVector
          )
        )
        _ = assert((firstDeployCost + secondDeployCost) == compoundDeployCost)
      } yield ()
    }
  }

  it should "just work" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      val genPostState = genesis.body.state.postStateHash
      val source =
        """
          #new d1,d2,d3,d4,d5,d6,d7,d8,d9 in {
          #  contract d1(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1) | d1!(depth - 1)  }
          #  } |
          #  contract d2(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1) | d2!(depth - 1)  }
          #  } |
          #  contract d3(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1) | d3!(depth - 1)  }
          #  } |
          #  contract d4(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1) | d4!(depth - 1)  }
          #  } |
          #  contract d5(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1) | d5!(depth - 1)  }
          #  } |
          #  contract d6(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1) | d6!(depth - 1)  }
          #  } |
          #  contract d7(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1) | d7!(depth - 1)  }
          #  } |
          #  contract d8(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) | d8!(depth - 1) }
          #  } |
          #  contract d9(@depth) = {
          #    if (depth <= 0) {
          #      Nil
          #    } else {
          #      d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) | d9!(depth - 1) }
          #  } |
          #  d1!(2) |
          #  d2!(2) |
          #  d3!(2) |
          #  d4!(2) |
          #  d5!(2) |
          #  d6!(2) |
          #  d7!(2) |
          #  d8!(2) |
          #  d9!(2)
          #}
          #""".stripMargin('#')
      ConstructDeploy.sourceDeployNowF(source = source, phloLimit = Int.MaxValue - 2) >>= {
        deploy =>
          computeState(runtimeManager, deploy, genPostState) >>= {
            case (playStateHash1, processedDeploy) =>
              replayComputeState(runtimeManager)(genPostState, processedDeploy) map {
                case Right(replayStateHash1) =>
                  assert(playStateHash1 != genPostState && replayStateHash1 == playStateHash1)
                case Left(replayFailure) => fail(s"Unexpected replay failure: $replayFailure")
              }
          }
      }
    }
  }

  private def invalidReplay(source: String): Task[Either[ReplayFailure, StateHash]] =
    runtimeManagerResource.use { runtimeManager =>
      for {
        deploy        <- ConstructDeploy.sourceDeployNowF(source, phloLimit = 10000)
        time          <- timeF.currentMillis
        genPostState  = genesis.body.state.postStateHash
        blockData     = BlockData(time, 0L, genesisContext.validatorPks.head, 0)
        invalidBlocks = Map.empty[BlockHash, Validator]
        newState <- runtimeManager
                     .computeState(genPostState)(
                       Seq(deploy),
                       Seq(
                         CloseBlockDeploy(
                           SystemDeployUtil
                             .generateCloseDeployRandomSeed(blockData.sender, blockData.seqNum)
                         )
                       ),
                       blockData,
                       invalidBlocks
                     )
        (_, processedDeploys, processedSystemDeploys) = newState
        processedDeploy                               = processedDeploys.head
        processedDeployCost                           = processedDeploy.cost.cost
        invalidProcessedDeploy = processedDeploy.copy(
          cost = PCost(processedDeployCost - 1)
        )
        result <- runtimeManager.replayComputeState(genPostState)(
                   Seq(invalidProcessedDeploy),
                   processedSystemDeploys,
                   blockData,
                   invalidBlocks,
                   isGenesis = false
                 )
      } yield result
    }

  "replayComputeState" should "catch discrepancies in initial and replay cost when no errors are thrown" in effectTest {
    invalidReplay("@0!(0) | for(@0 <- @0){ Nil }").map {
      case Left(ReplayCostMismatch(initialCost, replayCost)) =>
        assert(initialCost == 322L && replayCost == 323L)
      case _ => fail()
    }
  }

  "replayComputeState" should "not catch discrepancies in initial and replay cost when user errors are thrown" in effectTest {
    invalidReplay("@0!(0) | for(@x <- @0){ x.undefined() }").map {
      case Left(ReplayCostMismatch(initialCost, replayCost)) =>
        assert(initialCost == 9999L && replayCost == 10000L)
      case _ => fail()
    }
  }

  // This is additional test for sorting with joins and channels inside joins.
  // - after reverted PR https://github.com/rchain/rchain/pull/2436
  "joins" should "be replayed correctly" in effectTest {
    def hex(bs: ByteString) = Base16.encode(bs.toByteArray)

    val term =
      """
        |new a, b, c, d in {
        |  for (_ <- a; _ <- b) { Nil } |
        |  for (_ <- a; _ <- c) { Nil } |
        |  for (_ <- a; _ <- d) { Nil }
        |}
        |""".stripMargin

    val genPostState = genesis.body.state.postStateHash
    for {
      deploy <- ConstructDeploy.sourceDeployNowF(term)
      result <- runtimeManagerResource.use { rm =>
                 for {
                   time          <- timeF.currentMillis
                   blockData     = BlockData(time, 1L, genesisContext.validatorPks.head, 1)
                   invalidBlocks = Map.empty[BlockHash, Validator]
                   newState <- rm.computeState(genPostState)(
                                Seq(deploy),
                                Seq(),
                                blockData,
                                invalidBlocks
                              )
                   (stateHash, processedDeploys, processedSysDeploys) = newState
                   result <- rm.replayComputeState(genPostState)(
                              processedDeploys,
                              processedSysDeploys,
                              blockData,
                              invalidBlocks,
                              isGenesis = false
                            )
                 } yield (stateHash, result.right.get)
               }
      (playHash, replayHash) = result
      _                      = hex(playHash) shouldBe hex(replayHash)
    } yield ()
  }
}
