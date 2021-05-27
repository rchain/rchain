package coop.rchain.casper.batch1

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.TestNode._
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.merging.{BlockIndex, DagMerger}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.dag.InMemDAG
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.collection.immutable.ListMap

class StateMergerSpec extends FlatSpec with Matchers with Inspectors with MergeabilityRules {

  it should "respect base mergeability rules" in {
    baseMergeabilityCases.map(t => evalTestCase(t._1, t._2)).map(_.runSyncUnsafe())
  }

  it should "respect peak mergeability rules" in {
    peekMergeabilityCases.map(t => evalTestCase(t._1, t._2)).map(_.runSyncUnsafe())
  }

  // TODO remove this and enable joinMergeabilityCases
  it should "declare conflicts when produce touch join" in {
    touchingJoinShouldBeAlwaysConflicting.map(t => evalTestCase(t._1, t._2)).map(_.runSyncUnsafe())
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
    checkConflictAndMergeState(base, b1, b2, true, b1BaseMergedState, b2BaseMergedState)

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
    checkConflictAndMergeState(base, b1, b2, false, b1BaseMergedState, b2BaseMergedState)

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
      b1State: State,
      b2State: State
  )(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ): Effect[Boolean] = {

    case class MergingNode(index: BlockIndex, isFinalized: Boolean, postState: Blake2b256Hash)

    val deploys = Vector(
      ConstructDeploy.sourceDeploy(base.value, 1L, phloLimit = 500),
      ConstructDeploy.sourceDeploy(b1.value, 2L, phloLimit = 500),
      ConstructDeploy.sourceDeploy(b2.value, 3L, phloLimit = 500, sec = ConstructDeploy.defaultSec2)
    )
    implicit val concurrent                  = Concurrent[Task]
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

            leftIndex <- BlockIndex(
                          ByteString.copyFromUtf8("l"),
                          List(leftDeploy),
                          baseCheckpoint.root,
                          leftCheckpoint.root,
                          historyRepo
                        )
            rightIndex <- BlockIndex(
                           ByteString.copyFromUtf8("r"),
                           List(rightDeploy),
                           baseCheckpoint.root,
                           rightCheckpoint.root,
                           historyRepo
                         )
            baseIndex <- BlockIndex(
                          ByteString.EMPTY,
                          List.empty,
                          baseCheckpoint.root, // this does not matter
                          baseCheckpoint.root,
                          historyRepo
                        )
            leftNode  = MergingNode(leftIndex, isFinalized = false, leftCheckpoint.root)
            rightNode = MergingNode(rightIndex, isFinalized = false, rightCheckpoint.root)
            lfbNode   = MergingNode(baseIndex, isFinalized = true, baseCheckpoint.root)

            childMap   = ListMap(lfbNode -> Set(rightNode, leftNode))
            parentsMap = ListMap(rightNode -> Set(lfbNode), leftNode -> Set(lfbNode))
            dag        = new InMemDAG[Task, MergingNode](childMap, parentsMap)

            mergedState <- DagMerger.merge[Task, MergingNode](
                            List(leftNode, rightNode),
                            lfbNode,
                            dag,
                            _.isFinalized.pure[Task],
                            _.index.deployChains.pure[Task],
                            v => v.postState,
                            historyRepo
                          )
            mergedRoot        = Blake2b256Hash.fromByteString(mergedState._1)
            rejectedDeployOpt = mergedState._2.headOption
            referenceState = rejectedDeployOpt
              .map { v =>
                List((leftDeploy, b1State), (rightDeploy, b2State))
                  .filter {
                    case (deploy, _) => deploy.deploy.sig != v
                  }
                  .map { case (_, refState) => refState }
                  .head
              }
              .getOrElse(b1State)

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
                                              mergedRoot
                                            )
            _ = println(s"conflicts found: ${mergedState._2.size}")
            _ = println(s"base state: ${dataContinuationAtBaseState}")
            _ = println(s"main state: ${dataContinuationAtMainState}")
            _ = println(s"merging state: ${dataContinuationAtMergingState}")
            _ = println(s"merged state: ${dataContinuationAtMergedState}")
            _ = println(s"reference state: ${referenceState}")

            errMsg = s""" FAILED
                        | base = ${base.value}
                        | b1   = ${b1.value}
                        | b2   = ${b2.value}
                        | Conflict: ${rejectedDeployOpt.isDefined} should be ${isConflict} 
                        | Merged state 
                        | ${dataContinuationAtMergedState}
                        | should be
                        | ${referenceState}
                        |
                        | go see it at ${file.value}:${line.value}
                        | """.stripMargin
            _ <- Sync[Effect]
                  .raiseError(new Exception(errMsg))
                  .whenA(dataContinuationAtMergedState != referenceState)
          } yield true
      }
      .adaptError {
        case e: Throwable =>
          new TestFailedException(e, 5).severedAtStackDepth
      }
  }
}
