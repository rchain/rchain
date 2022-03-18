package coop.rchain.node.mergeablity

import cats.Monoid
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.merging.BlockIndex
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.node.mergeablity.RhoState.State
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.exceptions.TestFailedException

object RhoState {
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
      case State(dataOther, continuationOther, joinOther) =>
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
      case _ => false
    }
  }
  val emptyState: State = Monoid[State].empty
}

object OperationOn0Ch {
  case class Rho(
      value: String
  ) {
    val rstate: State      = state.runSyncUnsafe()
    def |(other: Rho): Rho = Rho(s"$value | ${other.value}")

    def state: Task[State] = {
      import coop.rchain.models.rholang.{implicits => toPar}
      implicit val logger: Log[Task]         = Log.log[Task]
      implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]
      implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
      rhoRuntimeEff[Task](initRegistry = false).use {
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

}

trait BasicMergeabilityRules extends ComputeMerge {

  import OperationOn0Ch.Rho
  import RhoState.State

  // this is just a note function which points out other testcases cover this one
  def CoveredBy(term: String): Unit = ()

  def MergeableCase(left: Rho*)(
      right: Rho*
  )(base: Rho*)(
      mergedState: State = State(Seq.empty, Seq.empty, Seq.empty)
  ): Unit =
    checkConflictAndMergeState(
      base.reduce(_ | _),
      right.reduce(_ | _),
      left.reduce(_ | _),
      isConflict = false,
      mergedState,
      rejectRight = false // this parameter is not actually important in merge case
    ).runSyncUnsafe()

  def ConflictingCase(left: Rho*)(
      right: Rho*
  )(base: Rho*)(
      mergedLeftState: State = State(Seq.empty, Seq.empty, Seq.empty)
  )(mergedRightState: State = State(Seq.empty, Seq.empty, Seq.empty)): Unit =
    (checkConflictAndMergeState(
      base.reduce(_ | _),
      left.reduce(_ | _),
      right.reduce(_ | _),
      isConflict = true,
      mergedRightState,
      rejectRight = false
    ) >> checkConflictAndMergeState(
      base.reduce(_ | _),
      left.reduce(_ | _),
      right.reduce(_ | _),
      isConflict = true,
      mergedLeftState,
      rejectRight = true
    )).runSyncUnsafe()

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
  ): Unit =
    ConflictingCase(left.reduce(_ | _))(right.reduce(_ | _))(base.reduce(_ | _))(mergedLeftState)(
      mergedRightState
    )

  /**
    * If there is infinite loop happened in a deploy, all the phlo would be taken and the deploy marked
    * as `errored`. Errored-deploys should make no changes to the state which should a mergeable case.
    */
  def InfiniteLoop(left: Rho*)(
      right: Rho*
  )(base: Rho*)(mergedState: State = State(Seq.empty, Seq.empty, Seq.empty)): Unit =
    MergeableCase(left.reduce(_ | _))(right.reduce(_ | _))(base.reduce(_ | _))(mergedState)

  private def getDataContinuationOnChannel0[F[_]: Sync](
      runtime: RhoRuntime[F],
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
      left: Rho,
      right: Rho,
      isConflict: Boolean,
      mergedStateResult: State,
      rejectRight: Boolean
  ): Task[Unit] = {

    case class MergingNode(index: BlockIndex, isFinalized: Boolean, postState: Blake2b256Hash)
    val shardId = ""
    val baseDeploy =
      ConstructDeploy.sourceDeploy(base.value, 1L, phloLimit = 500, shardId = shardId)
    val leftDeploy =
      ConstructDeploy.sourceDeploy(left.value, 2L, phloLimit = 500, shardId = shardId)
    val rightDeploy =
      ConstructDeploy.sourceDeploy(
        right.value,
        3L,
        phloLimit = 500,
        sec = ConstructDeploy.defaultSec2,
        shardId = shardId
      )
    implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val logger: Log[Task]         = Log.log[Task]
    computeMergeCase[Task](
      Seq(baseDeploy),
      Seq(leftDeploy),
      Seq(rightDeploy),
      (runtime, _, mergedState) => {
        val mergedRoot      = mergedState._1
        val rejectedDeploys = mergedState._2
        for {
          dataContinuationAtMergedState <- getDataContinuationOnChannel0(
                                            runtime,
                                            mergedRoot
                                          )
          errMsg = s""" FAILED
                  | base = ${baseDeploy}
                  | left   = ${leftDeploy}
                  | right   = ${rightDeploy}
                  | Conflict: ${rejectedDeploys.nonEmpty} should be ${isConflict} with ${rejectedDeploys}
                  | isRejectRight: ${rejectRight}
                  | Merged state
                  | ${dataContinuationAtMergedState}
                  | should be
                  | ${mergedStateResult}
                  |
                  | conflicts found: ${mergedState._2.size}
                  | """.stripMargin
          _ <- Sync[Task]
                .raiseError(new Exception(errMsg))
                .whenA(rejectedDeploys.isEmpty == isConflict)
          _ <- Sync[Task]
                .raiseError(new Exception(errMsg))
                .whenA(dataContinuationAtMergedState != mergedStateResult)
        } yield ()
      },
      rejectRight
    ).adaptError {
      case e: Throwable =>
        new TestFailedException(e, failedCodeStackDepth = 5).severedAtStackDepth
    }
  }
}
