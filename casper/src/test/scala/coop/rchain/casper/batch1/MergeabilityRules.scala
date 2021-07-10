package coop.rchain.casper.batch1

import cats.Monoid
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.merging.{BlockIndex, DagMerger}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.exceptions.TestFailedException

trait MergeabilityRules {

  /**
    * The mergeable cases below are from https://docs.google.com/spreadsheets/d/1pABqArF9e8HRTO9zSefp93mIVUm91avekeDgqSEw0R8/edit?usp=drive_web&ouid=115573881377148283245
    * The cases below are always following the pattern below.
    *     MergedBlock
    *   /           \
    * LeftBlock   RightBlock
    *   \           /
    *     BaseBlock
    *
    * `BaseBlock` is the start point of two branches which is trying to merge.
    * `LeftBlock` is the main state which would be take all the state in the merged block.
    * `RightBlock` is the block which is trying to merge into `LeftBlock`. If there is anything in `RightBlock` conflict
    * with `LeftBlock`, it would be rejected.
    *
    * For differences between cases like `4! C!` and `(4!) C1`
    *
    * 4! C!
    *           MergedBlock
    *       /               \
    * B2 Rho("@0!(1)")     B3 Rho("@0!(0)")
    *       \              /
    *             B1    Rho("contract @0(0) = { 0 } | for (@1 <- @0) { 0 }")
    *
    * (4!) C1
    *         MergedBlock
    *  /                      \
    * B2 Rho("@0!(1)| for (@1 <- @0) { 0 }")    B3 Rho("@0!(0)")
    *  \                      /
    *             B1  Rho("contract @0(0) = { 0 }")
    *
    */
  implicit val stateAdditionMonoid: Monoid[State] = new Monoid[State] {
    def empty: State                       = State(Seq.empty, Seq.empty, Seq.empty)
    def combine(x: State, y: State): State = x ++ y
  }

  case class State(
      datas: Seq[Datum[ListParWithRandom]],
      continuations: Seq[WaitingContinuation[BindPattern, TaggedContinuation]],
      joins: Seq[Seq[Par]]
  ) {

    override def toString = s"State:\n datas: ${datas}\n conts: ${continuations}\n joins: ${joins}"

    def ++(obj: State): State =
      State(
        datas ++ obj.datas,
        continuations ++ obj.continuations,
        // joins are not duplicate
        (joins.toSet ++ obj.joins.toSet).toSeq
      )

    private def continuationEqual(left: TaggedContinuation, right: TaggedContinuation): Boolean =
      left.taggedCont.parBody.get.body == right.taggedCont.parBody.get.body

    // equals ignoring source which is random based on the execution in data and continuation
    override def equals(obj: Any): Boolean = obj match {
      case State(dataOther, continuationOther, joinOther) => {
        val dataLengthEqual         = datas.length == dataOther.length
        val continuationLengthEqual = continuations.length == continuationOther.length
        val dataContentEqual = datas.forall(
          d =>
            dataOther.exists(p => p.a.pars == d.a.pars && p.persist == d.persist) && {
              dataOther.count(p => p.a.pars == d.a.pars && p.persist == d.persist) == datas
                .count(p => p.a.pars == d.a.pars && p.persist == d.persist)
            }
        )
        val continuationContentEqual = continuations.forall(
          c =>
            continuationOther.exists(
              p =>
                continuationEqual(p.continuation, c.continuation) && p.patterns == c.patterns &&
                  p.persist == c.persist && p.peeks == c.peeks
            ) && {
              continuationOther.count(
                p =>
                  continuationEqual(p.continuation, c.continuation) && p.patterns == c.patterns &&
                    p.persist == c.persist && p.peeks == c.peeks
              ) == continuations.count(
                p =>
                  continuationEqual(p.continuation, c.continuation) && p.patterns == c.patterns && p.persist == c.persist && p.peeks == c.peeks
              )
            }
        )
        val joinEqual = joins.toSet == joinOther.toSet
        dataLengthEqual && continuationLengthEqual && dataContentEqual && continuationContentEqual && joinEqual
      }
      case _ => false
    }
  }
  val emptyState = Monoid[State].empty

  trait MergeabilityTestCase {
    def left: Seq[Rho]
    def right: Seq[Rho]
    def base: Seq[Rho]
    def mergedLeftState: State
    def mergedRightState: State
    def run: Effect[Boolean]
  }

  case class CoveredBy(term: String) extends MergeabilityTestCase {
    override def run: Effect[Boolean] = true.pure[Effect]

    override val base: Seq[Rho]          = Seq.empty
    override val left: Seq[Rho]          = Seq.empty
    override val mergedLeftState: State  = State(Seq.empty, Seq.empty, Seq.empty)
    override val mergedRightState: State = State(Seq.empty, Seq.empty, Seq.empty)
    override val right: Seq[Rho]         = Seq.empty
  }

  /**
    * MergeableCase should be always the same for  `mergedLeftState` and `mergedRightState`
    */
  class MergeableCase(
      val left: Seq[Rho],
      val right: Seq[Rho],
      val base: Seq[Rho],
      val mergedLeftState: State,
      val mergedRightState: State
  ) extends MergeabilityTestCase {
    def run: Effect[Boolean] =
      merges(
        left.reduce(_ | _),
        right.reduce(_ | _),
        base.reduce(_ | _),
        mergedLeftState,
        mergedRightState
      )

  }
  object MergeableCase {
    def apply(left: Rho*)(
        right: Rho*
    )(base: Rho*)(
        mergedState: State = State(Seq.empty, Seq.empty, Seq.empty)
    ) = {
      val testCase =
        new MergeableCase(left.toSeq, right.toSeq, base.toSeq, mergedState, mergedState)
      testCase.run.runSyncUnsafe()
    }
  }

  class ConflictingCase(
      val left: Seq[Rho],
      val right: Seq[Rho],
      val base: Seq[Rho],
      val mergedLeftState: State = State(Seq.empty, Seq.empty, Seq.empty),
      val mergedRightState: State = State(Seq.empty, Seq.empty, Seq.empty)
  ) extends MergeabilityTestCase {
    def run: Effect[Boolean] =
      conflicts(
        left.reduce(_ | _),
        right.reduce(_ | _),
        base.reduce(_ | _),
        mergedLeftState,
        mergedRightState
      )

  }

  object ConflictingCase {
    def apply(left: Rho*)(
        right: Rho*
    )(base: Rho*)(
        mergedLeftState: State = State(Seq.empty, Seq.empty, Seq.empty)
    )(mergedRightState: State = State(Seq.empty, Seq.empty, Seq.empty)) = {
      val testCase =
        new ConflictingCase(left.toSeq, right.toSeq, base.toSeq, mergedLeftState, mergedRightState)
      testCase.run.runSyncUnsafe()
    }
  }

  /**
    * This is a mark for cases which happen left consume and right produce doesn't match.But because we don't run
    * pattern matching on merging right now, this is conflict now.
    *
    * Cases like below
    *                   MergedBlock
    *                 /               \
    * B2 Rho("for (@1 <- @0) { 0 }")     B3 Rho("@0!(0)")
    *                 \              /
    *    B1             Rho("Nil")
    */
  def CurrentConflictMergeableCase(left: Rho*)(
      right: Rho*
  )(base: Rho*)(mergedLeftState: State = State(Seq.empty, Seq.empty, Seq.empty))(
      mergedRightState: State = State(Seq.empty, Seq.empty, Seq.empty)
  ) = {
    val testCase = new ConflictingCase(left, right, base, mergedLeftState, mergedRightState)
    testCase.run.runSyncUnsafe()
  }

  /**
    * If there is infinite loop happened in a deploy, all the phlo would be taken and the deploy marked
    * as `errored`. Errored-deploys should make no changes to the state which should a mergeable case.
    */
  def InfiniteLoop(left: Rho*)(
      right: Rho*
  )(base: Rho*)(mergedState: State = State(Seq.empty, Seq.empty, Seq.empty)) = {
    val testCase = new MergeableCase(left, right, base, mergedState, mergedState)
    testCase.run.runSyncUnsafe()
  }

  case class Rho(
      value: String
  ) {

    val rstate             = state.runSyncUnsafe()
    def |(other: Rho): Rho = Rho(s"$value | ${other.value}")

    def state = {
      import coop.rchain.models.rholang.{implicits => toPar}
      implicit val logger: Log[Effect]         = Log.log[Task]
      implicit val metricsEff: Metrics[Effect] = new Metrics.MetricsNOP[Task]
      implicit val noopSpan: Span[Effect]      = NoopSpan[Task]()
      rhoRuntimeEff[Effect](initRegistry = false).use {
        case (runtime, _, _) =>
          for {
            _            <- runtime.evaluate(value, Cost(500L))
            _            <- runtime.createCheckpoint
            channel      = toPar(Expr(GInt(0)))
            data         <- runtime.getData(channel)
            continuation <- runtime.getContinuation(Seq(channel))
            joins        <- runtime.getJoins(channel)
          } yield State(data, continuation, joins)
      }
    }
  }
  object Nil extends Rho("Nil")

  // Sends (linear sends)
  val S0 = Rho("@0!(0)")
  val S1 = Rho("@0!(1)")

  // Repeats (persistent sends)
  val R0 = Rho("@0!!(0)")
  val R1 = Rho("@0!!(1)")

  // For-s (linear receives)
  val F_ = Rho("for (_ <- @0) { 0 }")
  val F0 = Rho("for (@0 <- @0) { 0 }")
  val F1 = Rho("for (@1 <- @0) { 0 }")

  // Peeks
  val P_ = Rho("for (_ <<- @0) { 0 }")
  val P0 = Rho("for (@0 <<- @0) { 0 }")
  val P1 = Rho("for (@1 <<- @0) { 0 }")

  // Contracts (persistent receives)
  val C_ = Rho("contract @0(id) = { 0 }")
  val C0 = Rho("contract @0(@0) = { 0 }")
  val C1 = Rho("contract @0(@1) = { 0 }")

  val S0on1 = Rho("@1!(0)")
  val S1on1 = Rho("@1!(1)")

  val J_  = Rho("for (_ <- @0;_ <- @1) { 0 }")
  val J0  = Rho("for (@0 <- @0;@0 <- @1) { 0 }")
  val J1  = Rho("for (@1 <- @0;@1 <- @1) { 0 }")
  val JC_ = Rho("for (_ <= @0;_ <= @1) { 0 }")
  val JC0 = Rho("for (@0 <= @0;@0 <= @1) { 0 }")
  val JC1 = Rho("for (@1 <= @0;@1 <= @1) { 0 }")

  def conflicts(
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

  def merges(
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

            leftIndex <- BlockIndex(
                          ByteString.copyFromUtf8("l"),
                          List(leftDeploy),
                          List.empty,
                          baseCheckpoint.root,
                          leftCheckpoint.root,
                          historyRepo
                        )
            rightIndex <- BlockIndex(
                           ByteString.copyFromUtf8("r"),
                           List(rightDeploy),
                           List.empty,
                           baseCheckpoint.root,
                           rightCheckpoint.root,
                           historyRepo
                         )
            baseIndex <- BlockIndex(
                          ByteString.EMPTY,
                          List.empty,
                          List.empty,
                          baseCheckpoint.root, // this does not matter
                          baseCheckpoint.root,
                          historyRepo
                        )
            kvm      = new InMemoryStoreManager
            dagStore <- BlockDagKeyValueStorage.create[Task](kvm)
            bBlock = getRandomBlock(
              setPreStateHash = RuntimeManager.emptyStateHashFixed.some,
              setPostStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setParentsHashList = List.empty.some
            )
            rBlock = getRandomBlock(
              setPreStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setPostStateHash = ByteString.copyFrom(rightCheckpoint.root.bytes.toArray).some,
              setParentsHashList = List(bBlock.blockHash).some
            )
            lBlock = getRandomBlock(
              setPreStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setPostStateHash = ByteString.copyFrom(leftCheckpoint.root.bytes.toArray).some,
              setParentsHashList = List(bBlock.blockHash).some
            )
            _   <- dagStore.insert(bBlock, false)
            _   <- dagStore.insert(lBlock, false)
            _   <- dagStore.insert(rBlock, false)
            dag <- dagStore.getRepresentation

            indices = Map(
              bBlock.blockHash -> baseIndex,
              rBlock.blockHash -> rightIndex,
              lBlock.blockHash -> leftIndex
            )
            mergedState <- DagMerger.merge[Task](
                            dag,
                            bBlock.blockHash,
                            baseCheckpoint.root,
                            indices(_).deployChains.pure[Task],
                            historyRepo
                          )
            mergedRoot        = mergedState._1
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
