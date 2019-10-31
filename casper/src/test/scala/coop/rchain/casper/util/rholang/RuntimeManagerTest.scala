package coop.rchain.casper.util.rholang

import cats.effect.Resource
import cats.syntax.all._
import cats.{Functor, Id}
import coop.rchain.casper.protocol.ProcessedSystemDeploy.Failed
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.SystemDeployPlatformFailure.UnexpectedSystemErrors
import coop.rchain.casper.util.rholang.SystemDeployPlayResult.{PlayFailed, PlaySucceeded}
import coop.rchain.casper.util.rholang.SystemDeployReplayResult.{ReplayFailed, ReplaySucceeded}
import coop.rchain.casper.util.rholang.costacc.{CheckBalance, PreChargeDeploy, RefundDeploy}
import coop.rchain.casper.util.{ConstructDeploy, GenesisBuilder, ProtoUtil}
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.PCost
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter
import coop.rchain.rholang.interpreter.Runtime.BlockData
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.{accounting, ParBuilderUtil}
import coop.rchain.shared.{Log, Time}
import coop.rchain.{metrics, rholang}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.concurrent.duration._

class RuntimeManagerTest extends FlatSpec with Matchers {

  implicit val timeF: Time[Task]         = new LogicalTime[Task]
  implicit val log: Log[Task]            = Log.log[Task]
  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

  val genesisContext = GenesisBuilder.buildGenesis()
  val genesis        = genesisContext.genesisBlock

  val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] = for {
    dirs <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    rm   <- Resources.mkRuntimeManagerAt[Task](dirs.rspaceDir)()
  } yield rm

  val runtimeAndManager: Resource[Task, (interpreter.Runtime[Task], RuntimeManager[Task])] = for {
    dirs <- Resources.copyStorage[Task](genesisContext.storageDirectory)
    rhr  <- rholang.Resources.mkRuntimeAt[Task](dirs.rspaceDir)()
    r    = rhr._1
    rm   <- Resource.liftF[Task, RuntimeManager[Task]](RuntimeManager.fromRuntime[Task](r))
  } yield (r, rm)

  private def computeState[F[_]: Functor](
      runtimeManager: RuntimeManager[F],
      deploy: DeployData,
      stateHash: StateHash
  ): F[(StateHash, ProcessedDeploy)] =
    for {
      res <- runtimeManager.computeState(stateHash)(
              deploy :: Nil,
              BlockData(deploy.timestamp, 0),
              Map.empty[BlockHash, Validator]
            )
      (hash, Seq(result)) = res
    } yield (hash, result)

  private def replayComputeState[F[_]: Functor](runtimeManager: RuntimeManager[F])(
      stateHash: StateHash,
      processedDeploy: ProcessedDeploy
  ): F[Either[ReplayFailure, StateHash]] =
    runtimeManager.replayComputeState(stateHash)(
      processedDeploy :: Nil,
      BlockData(processedDeploy.deploy.timestamp, 0),
      Map.empty[BlockHash, Validator],
      isGenesis = false
    )

  "computeState" should "charge for deploys" in effectTest {
    runtimeManagerResource.use { runtimeManager =>
      val userPk       = ConstructDeploy.defaultPub
      val genPostState = genesis.body.state.postStateHash
      compareSuccessfulSystemDeploys(runtimeManager)(genPostState)(
        new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(0.toByte))),
        new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(0.toByte)))
      )(_ == 9000000) >>= { stateHash0 =>
        val source = "@0!0"
        // TODO: Prohibit negative gas prices and gas limits in deploys.
        // TODO: Make minimum maximum yield for deploy parameter of node.
        ConstructDeploy.sourceDeployNowF(source = source, phloPrice = 1, phloLimit = 3) >>= {
          deploy =>
            computeState(runtimeManager, deploy, stateHash0) >>= {
              case (playStateHash1, processedDeploy) =>
                replayComputeState(runtimeManager)(stateHash0, processedDeploy) >>= {
                  case Right(replayStateHash1) =>
                    assert(playStateHash1 != genPostState && replayStateHash1 == playStateHash1)
                    compareSuccessfulSystemDeploys(runtimeManager)(playStateHash1)(
                      new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(1.toByte))),
                      new CheckBalance(pk = userPk, rand = Blake2b512Random(Array(1.toByte)))
                    )(_ == 9000000 - processedDeploy.cost.cost)
                  case Left(replayFailure) => fail(s"Unexpected replay failure: ${replayFailure}")
                }
            }
        }
      }
    }
  }

  private def invalidReplay(source: String): Task[Either[ReplayFailure, StateHash]] =
    runtimeManagerResource.use { runtimeManager =>
      for {
        deploy        <- ConstructDeploy.sourceDeployNowF(source)
        time          <- timeF.currentMillis
        genPostState  = genesis.body.state.postStateHash
        blockData     = BlockData(time, 0L)
        invalidBlocks = Map.empty[BlockHash, Validator]
        processedDeploys <- runtimeManager
                             .computeState(genPostState)(Seq(deploy), blockData, invalidBlocks)
                             .map(_._2)
        processedDeploy     = processedDeploys.head
        processedDeployCost = processedDeploy.cost.cost
        invalidProcessedDeploy = processedDeploy.copy(
          cost = PCost(processedDeployCost - 1)
        )
        result <- runtimeManager.replayComputeState(genPostState)(
                   Seq(invalidProcessedDeploy),
                   blockData,
                   invalidBlocks,
                   isGenesis = false
                 )
      } yield result
    }

  private def compareSuccessfulSystemDeploys[S <: SystemDeploy](
      runtimeManager: RuntimeManager[Task]
  )(startState: StateHash)(
      playSystemDeploy: S,
      replaySystemDeploy: S
  )(resultAssertion: S#Result => Boolean): Task[StateHash] =
    runtimeManager.playSystemDeploy(startState)(playSystemDeploy).attempt >>= {
      case Right(PlaySucceeded(finalPlayStateHash, processedSystemDeploy, playResult)) =>
        assert(resultAssertion(playResult))
        runtimeManager
          .replaySystemDeploy(startState)(replaySystemDeploy, processedSystemDeploy)
          .attempt
          .map {
            case Right(Right(systemDeployReplayResult)) =>
              systemDeployReplayResult match {
                case ReplaySucceeded(finalReplayStateHash, replayResult) =>
                  assert(finalPlayStateHash == finalReplayStateHash)
                  assert(playResult == replayResult)
                  finalReplayStateHash
                case ReplayFailed(systemDeployError) =>
                  fail(s"Unexpected user error during replay: ${systemDeployError.errorMessage}")
              }
            case Right(Left(replayFailure)) =>
              fail(s"Unexpected replay failure: $replayFailure")
            case Left(throwable) =>
              fail(s"Unexpected system error during replay: ${throwable.getMessage}")
          }
      case Right(PlayFailed(Failed(_, errorMsg))) =>
        fail(s"Unexpected user error during play: $errorMsg")
      case Left(throwable) =>
        fail(s"Unexpected system error during play: ${throwable.getMessage}")
    }

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
            term          <- ParBuilderUtil.buildNormalizedTerm[Task](deploy.term)
            _             <- runtime.reducer.inj(term)
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
    val purseValue     = "37"
    val captureChannel = "__PURSEVALUE__"

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
                    s""" for(nn <- @"nn"){ nn!("value", "$captureChannel") } """
                  )
        result1 <- mgr.captureResults(hash, deploy1, captureChannel)

        _ = result1.size should be(1)
        _ = result1.head should be(ParBuilderUtil.mkTerm(purseValue).right.get)
      } yield ()
    }
  }

  it should "handle multiple results and no results appropriately" in {
    val n    = 8
    val code = (1 to n).map(i => s""" @"__SCALA__"!($i) """).mkString("|")
    val term = ConstructDeploy.sourceDeploy(code, timestamp = 0)
    val manyResults =
      runtimeManagerResource
        .use(mgr => mgr.captureResults(mgr.emptyStateHash, term))
        .runSyncUnsafe(10.seconds)
    val noResults =
      runtimeManagerResource
        .use(mgr => mgr.captureResults(mgr.emptyStateHash, term, "differentName"))
        .runSyncUnsafe(10.seconds)

    noResults.isEmpty should be(true)

    manyResults.size should be(n)
    (1 to n).forall(i => manyResults.contains(ParBuilderUtil.mkTerm(i.toString).right.get)) should be(
      true
    )
  }

  "emptyStateHash" should "not remember previous hot store state" in effectTest {
    implicit val timeEff: LogicalTime[Id] = new LogicalTime[Id]

    val term = ConstructDeploy.basicDeployData[Id](0)

    def run: Task[StateHash] =
      runtimeManagerResource
        .use { m =>
          val hash = m.emptyStateHash
          computeState(m, term, genesis.body.state.postStateHash)
            .map(_ => hash)
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
        blockData     = BlockData(time, 0L)
        invalidBlocks = Map.empty[BlockHash, Validator]
        computeStateResult <- runtimeManager.computeState(genPostState)(
                               deploy :: Nil,
                               blockData,
                               invalidBlocks
                             )
        (playPostState, processedDeploys) = computeStateResult
        replayComputeStateResult <- runtimeManager.replayComputeState(genPostState)(
                                     processedDeploys,
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
        blockData     = BlockData(time, 0L)
        invalidBlocks = Map.empty[BlockHash, Validator]
        firstDeploy <- mgr
                        .computeState(genPostState)(deploy0 :: Nil, blockData, invalidBlocks)
                        .map(_._2)
        secondDeploy <- mgr
                         .computeState(genPostState)(deploy1 :: Nil, blockData, invalidBlocks)
                         .map(_._2)
        compoundDeploy <- mgr
                           .computeState(genPostState)(
                             deploy0 :: deploy1 :: Nil,
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

  "replayComputeState" should "catch discrepancies in initial and replay cost when no errors are thrown" in effectTest {
    invalidReplay("@0!(0) | for(@0 <- @0){ Nil }").map {
      case Left(ReplayCostMismatch(initialCost, replayCost)) =>
        assert(initialCost == 322L && replayCost == 323L)
      case _ => fail()
    }
  }

  "replayComputeState" should "not catch discrepancies in initial and replay cost when user errors are thrown" in effectTest {
    invalidReplay("@0!(0) | for(@x <- @0){ x.undefined() }").map {
      case Right(_) => succeed
      case _        => fail
    }
  }
}
