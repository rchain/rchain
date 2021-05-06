package coop.rchain.casper.batch1

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.casper.blocks.merger.{BlockIndex, Indexer, MergingVertex}
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.{ConstructDeploy, EventConverter}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.EventChain
import coop.rchain.rspace.merger.instances.EventsIndexConflictDetectors
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Log
import coop.rchain.store.LazyKeyValueCache
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FlatSpec, Inspectors, Matchers}

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
      mergedReferenceState: State
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
    rhoRuntimeEff[Effect](false)
      .use {
        case (runtime, _, historyRepo) =>
          println(s"""Running test for
               | base = ${base.value}
               | b1   = ${b1.value}
               | b2   = ${b2.value}""")
          for {
            baseDeploy          <- runtime.processDeploy(deploys(0))
            baseCheckpoint      <- runtime.createCheckpoint
            leftDeploy          <- runtime.processDeploy(deploys(1))
            leftCheckpoint @ _  <- runtime.createCheckpoint
            _                   <- runtime.reset(baseCheckpoint.root)
            rightDeploy         <- runtime.processDeploy(deploys(2))
            rightCheckpoint @ _ <- runtime.createCheckpoint

            blockIndexCache <- LazyKeyValueCache[Task, MergingVertex, BlockIndex](
                                Indexer.createBlockIndex[Task]
                              )
            leftV <- {
              implicit val ch = blockIndexCache
              Indexer.createBranchIndex(
                Seq(
                  MergingVertex(
                    postStateHash = leftCheckpoint.root.toByteString,
                    processedDeploys = Set(leftDeploy)
                  )
                )
              )
            }
            rightV <- {
              implicit val ch = blockIndexCache
              Indexer.createBranchIndex(
                Seq(
                  MergingVertex(
                    postStateHash = rightCheckpoint.root.toByteString,
                    processedDeploys = Set(rightDeploy)
                  )
                )
              )
            }

            baseStateReader = historyRepo.getHistoryReader(baseCheckpoint.root)
            // Caching readers for data in base state
            baseDataReader <- LazyKeyValueCache(
                               (ch: Blake2b256Hash) => {
                                 implicit val c = historyRepo
                                 baseStateReader
                                   .getDataFromChannelHash(ch)
                               }
                             )

            baseJoinReader <- LazyKeyValueCache(
                               (ch: Blake2b256Hash) => {
                                 implicit val c = historyRepo
                                 baseStateReader
                                   .getJoinsFromChannelHash(ch)
                               }
                             )

            conflicts <- EventsIndexConflictDetectors.findConflicts(
                          leftV.eventLogIndex,
                          rightV.eventLogIndex,
                          baseDataReader,
                          baseJoinReader
                        )
            _            = assert(conflicts.nonEmpty == isConflict)
            leftHistory  <- historyRepo.reset(leftCheckpoint.root).map(_.history)
            rightHistory <- historyRepo.reset(rightCheckpoint.root).map(_.history)
            baseHistory  <- historyRepo.reset(baseCheckpoint.root).map(_.history)
            mergedState <- historyRepo.stateMerger.merge(
                            leftHistory,
                            if (conflicts.nonEmpty) Seq.empty
                            else
                              Seq(
                                EventChain(
                                  startState = baseHistory,
                                  endState = rightHistory,
                                  events = rightDeploy.deployLog.map(EventConverter.toRspaceEvent)
                                )
                              )
                          )
            dataContinuationAtBaseState <- getDataContinuationOnChannel0(
                                            runtime,
                                            baseCheckpoint.root
                                          )
            dataContinuationAtMainState <- getDataContinuationOnChannel0(
                                            runtime,
                                            leftCheckpoint.root
                                          )
            dataContinuationAtMergingState <- getDataContinuationOnChannel0(
                                               runtime,
                                               rightCheckpoint.root
                                             )
            dataContinuationAtMergedState <- getDataContinuationOnChannel0(
                                              runtime,
                                              mergedState.root
                                            )
            _ = println(s"base state: ${dataContinuationAtBaseState}")
            _ = println(s"main state: ${dataContinuationAtMainState}")
            _ = println(s"merging state: ${dataContinuationAtMergingState}")
            _ = println(s"merged state: ${dataContinuationAtMergedState}")
            _ = println(s"reference state: ${mergedReferenceState}")
            _ <- Sync[Effect]
                  .raiseError(new Exception(dataContinuationAtMergedState.toString))
                  .whenA(dataContinuationAtMergedState != mergedReferenceState)
          } yield true
      }
      .adaptError {
        case e: Throwable =>
          new TestFailedException(s"""Expected
               | base = ${base.value}
               | b1   = ${b1.value}
               | b2   = ${b2.value}
               | The conflict result should be ${isConflict} and
               | the mergedState datas should be
               | ${mergedReferenceState}
               | and it is
               | ${e}
               |
               | go see it at ${file.value}:${line.value}
               | """.stripMargin, e, 5).severedAtStackDepth
      }
  }
}
