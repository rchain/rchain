package coop.rchain.casper.batch1

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.util.{ConstructDeploy, EventConverter}
import coop.rchain.casper.EstimatorHelper
import coop.rchain.casper.blocks.merger.{ConflictDetectors, MergingBranchIndex, MergingVertex}
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Log
import monix.eval.Task
import coop.rchain.casper.syntax._
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.Expr
import coop.rchain.rholang.interpreter.RhoRuntime
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.merger.EventChain
import coop.rchain.store.LazyKeyValueCache
import org.scalatest.exceptions.TestFailedException

class StateMergerSpec extends FlatSpec with Matchers with Inspectors with MergeabilityRules {

  it should "respect base mergeability rules" in {
    baseMergeabilityCases.map(t => evalTestCase(t._1, t._2)).map(_.runSyncUnsafe())
  }

  it should "respect peak mergeability rules" in {
    peekMergeabilityCases.map(t => evalTestCase(t._1, t._2)).map(_.runSyncUnsafe())
  }

  it should "respect join mergeability rules" ignore {
    joinMergeabilityCases.map(t => evalTestCase(t._1, t._2)).map(_.runSyncUnsafe())
  }

  def evalTestCase(testName: String, testCase: MergeabilityTestCase) =
    for {
      _ <- Task.delay(
            println(
              s"run test case ${testName}, with base ${testCase.base}, left: ${testCase.left}, right: ${testCase.right}"
            )
          )
      _ <- testCase.run
    } yield ()

  override def conflicts(
      b1: Rho,
      b2: Rho,
      base: Rho,
      b1BaseMergedState: State,
      b2BaseMergedState: State
  )(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    checkConflictAndMergeState(base, b1, b2, true, b1BaseMergedState) >>
      checkConflictAndMergeState(base, b2, b1, true, b2BaseMergedState)

  override def merges(
      b1: Rho,
      b2: Rho,
      base: Rho,
      b1BaseMergedState: State,
      b2BaseMergedState: State
  )(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ) =
    checkConflictAndMergeState(base, b1, b2, false, b1BaseMergedState) >>
      checkConflictAndMergeState(base, b2, b1, false, b2BaseMergedState)

  private def getDataContinuationOnChannel0(
      runtime: RhoRuntime[Effect],
      stateHash: Blake2b256Hash
  ) = {
    import coop.rchain.models.rholang.{implicits => toPar}
    for {
      _            <- runtime.reset(stateHash)
      data         <- runtime.getData(toPar(Expr(GInt(0))))
      continuation <- runtime.getContinuation(Seq(toPar(Expr(GInt(0)))))
      join         <- runtime.getJoins(toPar(Expr(GInt(0))))
    } yield State(data, continuation, join)
  }

  private[this] def checkConflictAndMergeState(
      base: Rho,
      b1: Rho,
      b2: Rho,
      isConflict: Boolean,
      mergedResultState: State
  )(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ): Effect[Boolean] = {

    val deploys = Vector(
      ConstructDeploy.sourceDeploy(base.value, 1L, phloLimit = 500),
      ConstructDeploy.sourceDeploy(b1.value, 2L, phloLimit = 500),
      ConstructDeploy.sourceDeploy(b2.value, 3L, phloLimit = 500, sec = ConstructDeploy.defaultSec2)
    )
    implicit val concurent                   = Concurrent[Task]
    implicit val metricsEff: Metrics[Effect] = new Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Effect]      = NoopSpan[Task]()
    implicit val logger: Log[Effect]         = Log.log[Task]
    import coop.rchain.rholang.interpreter.storage._
    rhoRuntimeEff[Effect](false)
      .use {
        case (runtime, _, historyRepo) =>
          for {
            baseDeploy @ _      <- runtime.processDeploy(deploys(0))
            baseCheckpoint      <- runtime.createCheckpoint
            leftDeploy          <- runtime.processDeploy(deploys(1))
            leftCheckpoint @ _  <- runtime.createCheckpoint
            _                   <- runtime.reset(baseCheckpoint.root)
            rightDeploy         <- runtime.processDeploy(deploys(2))
            rightCheckpoint @ _ <- runtime.createCheckpoint
            stateMerger         <- historyRepo.stateMerger

            leftV <- MergingBranchIndex.create(
                      Seq(
                        MergingVertex(
                          postStateHash = leftCheckpoint.root.toByteString,
                          processedDeploys = Set(leftDeploy)
                        )
                      )
                    )
            rightV <- MergingBranchIndex.create(
                       Seq(
                         MergingVertex(
                           postStateHash = rightCheckpoint.root.toByteString,
                           processedDeploys = Set(rightDeploy)
                         )
                       )
                     )
            baseStateReader <- historyRepo.reset(baseCheckpoint.root)
            // Caching readers for data in base state
            baseDataReader <- LazyKeyValueCache(
                               (ch: Blake2b256Hash) =>
                                 baseStateReader
                                   .getDataFromChannelHash(ch)
                             )
            baseJoinReader <- LazyKeyValueCache(
                               (ch: Blake2b256Hash) =>
                                 baseStateReader
                                   .getJoinsFromChannelHash(ch)
                             )
            conflicts <- ConflictDetectors.findConflicts(
                          leftV,
                          rightV,
                          baseDataReader,
                          baseJoinReader
                        )
            _ = assert(conflicts.nonEmpty == isConflict)
            mergedState <- stateMerger.merge(
                            leftCheckpoint.root,
                            if (conflicts.nonEmpty) Seq.empty
                            else
                              Seq(
                                EventChain(
                                  startState = baseCheckpoint.root,
                                  endState = rightCheckpoint.root,
                                  events = rightDeploy.deployLog.map(EventConverter.toRspaceEvent)
                                )
                              )
                          )
            dataContinuationAtMergedState <- getDataContinuationOnChannel0(runtime, mergedState)
            _ <- Sync[Effect]
                  .raiseError(new Exception(dataContinuationAtMergedState.toString))
                  .whenA(dataContinuationAtMergedState != mergedResultState)
          } yield true
      }
      .adaptError {
        case e: Throwable =>
          new TestFailedException(s"""Expected
                                     | base = ${base.value}
                                     | b1   = ${b1.value}
                                     | b2   = ${b2.value}
                                     | The conflict result should be ${isConflict} and
                                     | the mergedState should be
                                     | ${mergedResultState}
                                     | and it is
                                     | ${e}
                                     |
                                     | go see it at ${file.value}:${line.value}
                                     | """.stripMargin, e, 5).severedAtStackDepth
      }
  }
}
