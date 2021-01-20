package coop.rchain.casper.batch1

import cats.Monoid
import cats.implicits._
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.rholang.interpreter.syntax._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.{BindPattern, Expr, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

trait MergeabilityRules {
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
    ) =
      new MergeableCase(left.toSeq, right.toSeq, base.toSeq, mergedState, mergedState)
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
    )(mergedRightState: State = State(Seq.empty, Seq.empty, Seq.empty)) =
      new ConflictingCase(left.toSeq, right.toSeq, base.toSeq, mergedLeftState, mergedRightState)
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
  ) =
    new ConflictingCase(left, right, base, mergedLeftState, mergedRightState)

  /**
    * If there is infinite loop happened in a deploy, all the phlo would be taken and the deploy marked
    * as `errored`. Errored-deploys should make no changes to the state which should a mergeable case.
    */
  def InfiniteLoop(left: Rho*)(
      right: Rho*
  )(base: Rho*)(mergedState: State = State(Seq.empty, Seq.empty, Seq.empty)) =
    new MergeableCase(left, right, base, mergedState, mergedState)

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
        case (runtime, _) =>
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
  import coop.rchain.models.rholang.{implicits => toPar}

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
  val baseMergeabilityCases = List(
    "!X !X"   -> MergeableCase(S0)(S0)(Nil)(S0.rstate ++ S0.rstate),
    "!X !4"   -> MergeableCase(S0)(F1)(S1)(S0.rstate),
    "!X (!4)" -> MergeableCase(S0)(S0, F_)(Nil)(S0.rstate),
//    "!X !C"    -> ConflictingCase(S0)(C1)(S1)(S1.rstate ++ S0.rstate)(C1.rstate),
//    "!X (!C)"  -> ConflictingCase(S0)(S0, C_)(Nil)(S0.rstate)(C_.rstate),
//    "!X 4X"    -> ConflictingCase(S0)(F_)(Nil)(S0.rstate)(F_.rstate),
//    "!X 4!"    -> ConflictingCase(S0)(S0)(F_)(emptyState)(emptyState),
    "!X 4!"    -> MergeableCase(S1)(S0)(S1, F0)(S1.rstate ++ S1.rstate),
    "!X (4!)"  -> CoveredBy("!X (!4)"),
    "!X 4!!"   -> MergeableCase(S1)(R0)(F0)(S1.rstate ++ R0.rstate),
    "!X (4!!)" -> MergeableCase(S0)(F_, R0)(Nil)(S0.rstate ++ R0.rstate),
    "!X !!X"   -> MergeableCase(S0)(R0)(Nil)(S0.rstate ++ R0.rstate),
    "!X !!4"   -> MergeableCase(S0)(F1)(R1)(S0.rstate ++ R1.rstate),
    "!X (!!4)" -> CoveredBy("!X (4!!)"),
    //"!X !!C"   -> InfiniteLoop(S0)(C1)(R1)(S0.rstate ++ R1.rstate),
//    "!X (!!C)" -> InfiniteLoop(F1)(C0, R0)(S0)(S0.rstate ++ F1.rstate),
//    "!X C!!"   -> InfiniteLoop(F1)(R1)(S0, C1)(C1.rstate ++ F1.rstate ++ S0.rstate),
    "!X CX"   -> ConflictingCase(S0)(C_)(Nil)(S0.rstate)(C_.rstate),
    "!X (C!)" -> CoveredBy("!X (!C)"),
//    "!4 !4"      -> NonDeterminedUnknownCase(F_)(F_)(S1, S0)(emptyState)(emptyState),
    "!4 !4" -> MergeableCase(F0)(F1)(S0, S1)(emptyState),
//    "!4 (!4)"    -> NonDeterminedUnknownCase(F0)(S1, F_)(S0)(emptyState)(emptyState),
    "!4 (!4)"    -> MergeableCase(F0)(S1, F1)(S0)(emptyState),
    "(!4) (!4)"  -> MergeableCase(S0, F_)(S0, F_)(Nil)(emptyState),
    "(!4) !4"    -> CoveredBy("!4 (!4)"),
    "!4 !C"      -> MergeableCase(F0)(C1)(S0, S1)(C1.rstate),
    "(!4) (!C)"  -> CoveredBy("(!4) !C"),
    "(!4) !C"    -> MergeableCase(F0, S0)(C1)(S1)(C1.rstate),
    "!4 (!C)"    -> MergeableCase(F0)(C1, S1)(S0)(C1.rstate),
    "!4 4X"      -> MergeableCase(F0)(Nil)(S0, F1)(F1.rstate),
    "(!4) 4X"    -> MergeableCase(S0, F_)(Nil)(F1)(F1.rstate),
    "!4 4!"      -> MergeableCase(S0)(F_)(F0, S1)(emptyState),
    "(!4) (4!)"  -> MergeableCase(S0, F_)(S0, F_)(Nil)(emptyState),
    "!4 (4!)"    -> CoveredBy("!4 (!4)"),
    "(!4) 4!"    -> MergeableCase(S1, F1)(S0)(F0)(emptyState),
    "!4 4!!"     -> MergeableCase(F0)(R1)(S0, F1)(R1.rstate),
    "(!4) 4!!"   -> MergeableCase(F0, S0)(R1)(F1)(R1.rstate),
    "!4 (4!!)"   -> MergeableCase(F0)(R1, F1)(S0)(R1.rstate),
    "(!4) (4!!)" -> MergeableCase(F0, S0)(R1, F1)(Nil)(R1.rstate),
    "!4 !!X"     -> MergeableCase(F0)(Nil)(S0, R1)(R1.rstate),
    "(!4) !!X"   -> MergeableCase(F0, S0)(F0)(R1)(R1.rstate ++ F0.rstate),
//    "!4 !!4"     -> NonDeterminedUnknownCase(F_)(F_)(S0, R1)(emptyState)(emptyState),
    "!4 !!4"   -> MergeableCase(F0)(F1)(S0, R1)(R1.rstate),
    "(!4) !!4" -> MergeableCase(F0, S0)(F_)(R1)(R1.rstate),
//    "!4 (!!4)"   -> NonDeterminedConflictCase(F_)(F1, R1)(S0)(emptyState)(R1.rstate ++ S0.rstate),
    "!4 (!!4)"   -> MergeableCase(F0)(F1, R1)(S0)(R1.rstate),
    "(!4) (!!4)" -> MergeableCase(F0, S0)(F1, R1)(Nil)(R1.rstate),
//    "!4 !!C"     -> InfiniteLoop(F0)(C1)(S0, R1)(R1.rstate),
//    "(!4) !!C"   -> InfiniteLoop(F0, S0)(C1)(R1)(R1.rstate),
//    "!4 (!!C)"   -> InfiniteLoop(F0)(C1, R1)(S0)(emptyState),
//    "(!4) (C!!)" -> InfiniteLoop(F0, S0)(R1, C1)(Nil)(emptyState),
    "!4 CX"     -> MergeableCase(F_)(Nil)(S0, C1)(C1.rstate),
    "(!4) CX"   -> MergeableCase(F1, S1)(S1)(C0)(C0.rstate ++ S1.rstate),
    "!4 C!"     -> MergeableCase(F0)(S1)(S0, C1)(C1.rstate),
    "!4 (C!)"   -> ConflictingCase(F0)(C_, S1)(S0)(emptyState)(C_.rstate),
    "(!4) (C!)" -> MergeableCase(S0, F_)(C_, S0)(Nil)(C_.rstate),
//    "(!4) C!"    -> NonDeterminedMergeableCase(S0, F0)(S1)(C_)(C_.rstate)(C_.rstate),
//    "!4 C!!"     -> InfiniteLoop(F0)(R1)(S0, C1)(C1.rstate),
//    "(!4) C!!"   -> InfiniteLoop(F0, S0)(R1)(C1)(C1.rstate),
    "!4 (C!!)"   -> CoveredBy("!4 (!!C)"),
    "(!4) (C!!)" -> CoveredBy("(!4) (C!!)"),
    "!C !C"      -> MergeableCase(C0)(C1)(S0, S1)(C0.rstate ++ C1.rstate),
    "!C !C"      -> ConflictingCase(C_)(C_)(S0, S1)(C_.rstate)(C_.rstate),
    "!C (!C)"    -> ConflictingCase(C0)(C_, S1)(S0)(C0.rstate)(C_.rstate),
    "!C (!C)"    -> MergeableCase(C0)(C1, S1)(S0)(C0.rstate ++ C1.rstate),
    "(!C) !C"    -> CoveredBy("!C (!C)"),
    "(!C) (!C)"  -> MergeableCase(S0, C_)(S0, C_)(Nil)(C_.rstate ++ C_.rstate),
    "!C 4X"      -> MergeableCase(C_)(Nil)(S0, F1)(C_.rstate ++ F1.rstate),
    "(!C) 4X"    -> MergeableCase(S0, C_)(F_)(Nil)(C_.rstate ++ F_.rstate),
    "!C 4!"      -> MergeableCase(C0)(S1)(S0, F1)(C0.rstate),
//    "(!C) 4!"   -> NonDeterminedConflictCase(S0, C_)(S0)(F_)(C_.rstate ++ F_.rstate)(emptyState),
    "!C (4!)"   -> MergeableCase(C0)(F1, S1)(S0)(C0.rstate),
    "(!C) (4!)" -> MergeableCase(S0, C_)(F_, S0)(Nil)(C_.rstate),
    "!C 4!!" -> CurrentConflictMergeableCase(C0)(R1)(S0, F1)(C0.rstate ++ F1.rstate)(
      R1.rstate ++ S0.rstate
    ),
    "!C 4!! 2" -> ConflictingCase(C_)(R1)(S0, F1)(C_.rstate ++ F1.rstate)(
      R1.rstate ++ S0.rstate
    ),
    "(!C) 4!!"     -> CurrentConflictMergeableCase(C0, S0)(R1)(F1)(C0.rstate ++ F1.rstate)(R1.rstate),
    "(!C) 4!! 2"   -> ConflictingCase(C_, S0)(R1)(F1)(C_.rstate ++ F1.rstate)(R1.rstate),
    "!C (4!!)"     -> CurrentConflictMergeableCase(C0)(R1, F1)(S0)(C0.rstate)(R1.rstate ++ S0.rstate),
    "!C (4!!) 2"   -> ConflictingCase(C_)(R1, F1)(S0)(C_.rstate)(R1.rstate ++ S0.rstate),
    "(!C) (4!!)"   -> CurrentConflictMergeableCase(C0, S0)(R1, F1)(Nil)(C0.rstate)(R1.rstate),
    "(!C) (4!!) 2" -> ConflictingCase(C_, S0)(R1, F1)(Nil)(C_.rstate)(R1.rstate),
    "!C !!X"       -> MergeableCase(C0)(Nil)(S0, R1)(C0.rstate ++ R1.rstate),
    "(!C) !!X"     -> MergeableCase(C0, S0)(Nil)(R1)(C0.rstate ++ R1.rstate),
    "!C !!4"       -> MergeableCase(C0)(F1)(S0, R1)(C0.rstate ++ R1.rstate),
//    "!C !!C"       -> InfiniteLoop(C0)(C1)(S0, R1)(R1.rstate ++ C0.rstate),
//    "(!C) !!C"     -> InfiniteLoop(C0, S0)(C1)(R1)(R1.rstate ++ C0.rstate),
//    "!C (!!C)"     -> InfiniteLoop(C0)(C1, R1)(S0)(C0.rstate),
//    "(!C) (!!C)"   -> InfiniteLoop(C0, S0)(C1, R1)(Nil)(C0.rstate),
    "!C CX"     -> MergeableCase(C0)(Nil)(S0, C1)(C0.rstate ++ C1.rstate),
    "(!C) CX"   -> MergeableCase(S0, C0)(C_)(Nil)(C0.rstate ++ C_.rstate),
    "!C C!"     -> MergeableCase(S0)(C_)(C0, S1)(C_.rstate ++ C0.rstate),
    "(!C) C!"   -> ConflictingCase(S0, C_)(C_)(S0)(C_.rstate)(C_.rstate),
    "!C (C!)"   -> CoveredBy("!C (!C)"),
    "(!C) (C!)" -> MergeableCase(S0, C_)(C_, S0)(Nil)(C_.rstate ++ C_.rstate),
//    "!C C!!"       -> InfiniteLoop(C0)(R1)(S0, C1)(C1.rstate ++ C0.rstate),
//    "(!C) C!!"     -> InfiniteLoop(C0, S0)(R1)(C1)(C1.rstate ++ C0.rstate),
    "!C (C!!)"   -> CoveredBy("!C (!!C)"),
    "(!C) (C!!)" -> CoveredBy("(!C) (!!C)"),
    "4X 4X"      -> MergeableCase(F_)(F_)(Nil)(F_.rstate ++ F_.rstate),
    "4X 4!"      -> MergeableCase(F0)(F_)(S1)(F0.rstate),
    "4X (4!)"    -> MergeableCase(F_)(F_, S0)(Nil)(F_.rstate),
    "4X 4!!"     -> MergeableCase(S1)(R0)(F0)(S1.rstate ++ R0.rstate),
    "4X (4!!)"   -> CurrentConflictMergeableCase(F1)(R0, F0)(Nil)(F1.rstate)(R0.rstate),
    "4X (4!!) 2" -> ConflictingCase(F0)(R0, F0)(Nil)(F0.rstate)(R0.rstate),
    "4X !!X" -> CurrentConflictMergeableCase(S0)(F1)(F1, R0)(F1.rstate ++ S0.rstate ++ R0.rstate)(
      F1.rstate ++ F1.rstate ++ R0.rstate
    ),
    "4X !!X 2" -> ConflictingCase(F_)(R0, F1)(Nil)(F_.rstate)(R0.rstate ++ F1.rstate),
    "4X !!4"   -> MergeableCase(F0)(F1)(R1)(F0.rstate ++ R1.rstate),
    "4X (!!4)" -> ConflictingCase(F_)(F1, R1)(Nil)(F_.rstate)(R1.rstate),
//    "4X !!C"   -> InfiniteLoop(S1)(C1)(F0, R1)(S1.rstate ++ F0.rstate ++ R1.rstate),
//    "4X (!!C)" -> InfiniteLoop(S1)(C1, R1)(F0)(S1.rstate ++ F0.rstate),
    "4X CX"   -> MergeableCase(F_)(C_)(Nil)(F_.rstate ++ C_.rstate),
    "4X C!"   -> MergeableCase(F0)(S1)(C1)(F0.rstate ++ C1.rstate),
    "4X (C!)" -> MergeableCase(F_)(C_, S0)(Nil)(F_.rstate ++ C_.rstate),
//    "4X C!!"     -> NonDeterminedMergeableCase(S0)(R0)(F1, C0)(C0.rstate)(C0.rstate),
    "4! 4!"     -> ConflictingCase(S0)(S1)(F_)(emptyState)(emptyState),
    "4! 4!"     -> MergeableCase(S0)(S1)(F0, F1)(emptyState),
    "(4!) (4!)" -> MergeableCase(F_, S0)(F_, S0)(Nil)(emptyState),
//    "(4!) 4!"    -> NonDeterminedUnknownCase(F1, S1)(S0)(F_)(emptyState)(emptyState),
    "(4!) 4!"    -> MergeableCase(F1, S1)(S0)(F0)(emptyState),
    "4! (4!)"    -> CoveredBy("(4!) 4!"),
    "4! 4!!"     -> MergeableCase(S0)(R1)(F0, F1)(R1.rstate),
    "(4!) (4!!)" -> MergeableCase(F0, S0)(F1, R1)(Nil)(R1.rstate),
    "(4!) 4!!"   -> MergeableCase(F0, S0)(R1)(F1)(R1.rstate),
    "4! (4!!)"   -> MergeableCase(S0)(F1, R1)(F0)(R1.rstate),
    "4! !!X"     -> MergeableCase(S0)(R1)(F0)(R1.rstate),
    "(4!) !!X"   -> MergeableCase(S0, F0)(R1)(Nil)(R1.rstate),
    "4! !!4"     -> MergeableCase(S1)(F0)(F1, R0)(R0.rstate),
    "(4!) !!4"   -> MergeableCase(S1, F1)(F0)(R0)(R0.rstate),
    "(4!) (!!4)" -> CoveredBy("(4!) (4!!)"),
    "4! (!!4)"   -> MergeableCase(S1)(F0, R0)(F1)(R0.rstate),
//    "4! !!C"     -> InfiniteLoop(S1)(C0)(F1, R0)(R0.rstate),
//    "(4!) !!C"   -> InfiniteLoop(S1, F1)(C0)(R0)(R0.rstate),
//    "4! (!!C)"   -> InfiniteLoop(S1)(C0, R0)(F1)(emptyState),
//    "(4!) (!!C)" -> InfiniteLoop(S1, F1)(C0, R0)(Nil)(emptyState),
    "4! CX"   -> MergeableCase(S0)(C1)(F_)(C1.rstate),
    "(4!) CX" -> MergeableCase(F_, S0)(C_)(Nil)(C_.rstate),
//    "4! C!"      -> NonDeterminedConflictCase(S0)(S1)(F_, C_)(emptyState)(emptyState),
    "4! C!" -> MergeableCase(S0)(S1)(F0, C1)(C1.rstate), // double check
//    "4! (C!)"    -> NonDeterminedConflictCase(S0)(C_, S0)(F_)(emptyState)(C_.rstate),
    "(4!) C!" -> MergeableCase(F1, S1)(S0)(C0)(C0.rstate),
//    "(4!) C!"    -> NonDeterminedConflictCase(F1, S1)(S0)(C_)(C_.rstate)(C_.rstate),
    "(4!) (C!)" -> MergeableCase(F_, S0)(C_, S0)(Nil)(C_.rstate),
//    "4! C!!"      -> InfiniteLoop(S1)(R0)(F1, C0)(C0.rstate),
//    "(4!) C!!"    -> InfiniteLoop(S1, F1)(R0)(C0)(C0.rstate),
    "4! (C!!)"    -> CoveredBy("4! (!!C)"),
    "(4!) (C!!)"  -> CoveredBy("(4!) (!!C)"),
    "4!! 4!!"     -> ConflictingCase(R1)(R0)(F_)(R1.rstate)(R0.rstate),
    "4!! 4!!"     -> MergeableCase(R1)(R0)(F1, F0)(R1.rstate ++ R0.rstate),
    "(4!!) (4!!)" -> MergeableCase(R1, F1)(R0, F0)(Nil)(R1.rstate ++ R0.rstate),
    "(4!!) 4!!"   -> ConflictingCase(R1, F1)(R0)(F_)(R1.rstate)(R0.rstate),
    "(4!!) 4!!"   -> MergeableCase(R1, F1)(R0)(F0)(R1.rstate ++ R0.rstate),
    "4!! (4!!)"   -> ConflictingCase(R1)(R0, F0)(F_)(R1.rstate)(R0.rstate),
    "4!! !!X"     -> MergeableCase(R1)(Nil)(F1, R0)(R1.rstate ++ R0.rstate),
    "4!! !!X" -> ConflictingCase(R1)(F1)(F1, R0)(R1.rstate ++ R0.rstate)(
      F1.rstate ++ F1.rstate ++ R0.rstate
    ),
    "(4!!) !!X"   -> ConflictingCase(R1, F1)(F1)(R0)(R1.rstate ++ R0.rstate)(R0.rstate ++ F1.rstate),
    "4!! !!4"     -> MergeableCase(R0)(F1)(F0, R1)(R1.rstate ++ R0.rstate),
    "4!! !!4"     -> MergeableCase(R0)(F_)(F0, R1)(R1.rstate ++ R0.rstate),
    "(4!!) !!4"   -> MergeableCase(R0, F0)(F1)(R1)(R1.rstate ++ R0.rstate),
    "(4!!) !!4"   -> MergeableCase(R0, F_)(F1)(R1)(R1.rstate ++ R0.rstate),
    "4!! (!!4)"   -> CoveredBy("(4!!) !!4"),
    "(4!!) (!!4)" -> MergeableCase(R0, F0)(F1, R1)(Nil)(R1.rstate ++ R0.rstate),
    "(4!!) (!!4)" -> MergeableCase(R0, F_)(F1, R1)(Nil)(R1.rstate ++ R0.rstate),
//    "4!! !!C"     -> InfiniteLoop(R1)(C0)(F1, R0)(R1.rstate ++ R0.rstate),
//    "(4!!) !!C"   -> InfiniteLoop(R1, F1)(C0)(R0)(R1.rstate ++ R0.rstate),
//    "4!! (!!C)"   -> InfiniteLoop(R1)(C0, R0)(F1)(R1.rstate),
//    "(4!!) (!!C)" -> InfiniteLoop(R1, F1)(C0, R0)(Nil)(R1.rstate),
    "4!! CX"     -> CurrentConflictMergeableCase(R1)(C0)(F1)(R1.rstate)(C0.rstate ++ F1.rstate),
    "4!! CX 2"   -> ConflictingCase(R1)(C_)(F1)(R1.rstate)(C_.rstate ++ F1.rstate),
    "(4!!) CX"   -> ConflictingCase(R1, F1)(C_)(Nil)(R1.rstate)(C_.rstate),
    "(4!!) CX 2" -> CurrentConflictMergeableCase(R1, F1)(C0)(Nil)(R1.rstate)(C0.rstate),
    "4!! C!"     -> MergeableCase(R1)(S0)(F1, C0)(R1.rstate ++ C0.rstate),
//    "(4!!) C!"     -> NonDeterminedConflictCase(R1, F_)(C0)(S0)(R1.rstate)(C0.rstate),
//    "(4!!) C! 2"   -> NonDeterminedMergeableCase(R1, F_)(C_)(S0)(emptyState)(emptyState),
    "4!! (C!)"     -> CurrentConflictMergeableCase(R1)(C0, S0)(F1)(R1.rstate)(C0.rstate ++ F1.rstate),
    "4!! (C!) 2"   -> ConflictingCase(R1)(C_, S0)(F1)(R1.rstate)(C_.rstate ++ F1.rstate),
    "(4!!) (C!)"   -> CurrentConflictMergeableCase(R1, F1)(C0, S0)(Nil)(R1.rstate)(C0.rstate),
    "(4!!) (C!) 2" -> ConflictingCase(R1, F1)(C_, S0)(Nil)(R1.rstate)(C_.rstate),
    ///@@@@@@@@@@@@@
    //  things needs to be confirm -> "@0!!(0) | @!(0)"
//    "4!! C!!"      -> InfiniteLoop(R1)(R0)(F1, C0)(R1.rstate ++ C0.rstate),
//    "(4!!) C!!"    -> InfiniteLoop(R1, F1)(R0)(C0)(R1.rstate ++ C0.rstate),
    "4!! (C!!)"   -> CoveredBy("4!! (!!C)"),
    "(4!!) (C!!)" -> CoveredBy("(4!!) (!!C)"),
    "!!X !!X"     -> MergeableCase(Nil)(Nil)(R0, R1)(R0.rstate ++ R1.rstate),
    "!!X !!4"     -> MergeableCase(Nil)(F1)(R0, R1)(R0.rstate ++ R1.rstate),
    "!!X (!!4)"   -> MergeableCase(Nil)(F1, R1)(R0)(R0.rstate ++ R1.rstate),
//    "!!X !!C"      -> InfiniteLoop(R0)(C1)(R1)(R0.rstate ++ R1.rstate),
//    "!!X (!!C)"    -> InfiniteLoop(R0)(C1, R1)(Nil)(R0.rstate),
    "!!X CX"   -> ConflictingCase(R0)(C_)(Nil)(R0.rstate)(C_.rstate),
    "!!X C!"   -> MergeableCase(F1)(S1)(R0, C1)(R0.rstate ++ F1.rstate ++ C1.rstate),
    "!!X (C!)" -> MergeableCase(F1)(S1, C1)(R0)(R0.rstate ++ F1.rstate ++ C1.rstate),
//    "!!X C!!"      -> InfiniteLoop(R0)(R1)(C1)(R0.rstate ++ C1.rstate),
    "!!X (C!!)"   -> CoveredBy("!!X (!!C)"),
    "!!4 !!4"     -> MergeableCase(F0)(F1)(R1, R0)(R1.rstate ++ R0.rstate),
    "!!4 !!4 2"   -> MergeableCase(F_)(F1)(R1, R0)(R1.rstate ++ R0.rstate),
    "!!4 (!!4)"   -> ConflictingCase(F1)(F_, R1)(R0)(F1.rstate ++ R0.rstate)(R1.rstate ++ R0.rstate),
    "(!!4) !!4"   -> CoveredBy("!!4 (!!4)"), // new
    "(!!4) (!!4)" -> MergeableCase(F1, R1)(F0, R0)(Nil)(R1.rstate ++ R0.rstate),
//    "!!4 !!C"      -> InfiniteLoop(F1)(C0)(R1, R0)(R1.rstate ++ R0.rstate),
    "(!!4) !!C" -> CoveredBy("(4!!) !!C"),
//    "!!4 (!!C)"    -> InfiniteLoop(F1)(C0, R0)(R1)(R1.rstate),
    "(!!4) (!!C)"  -> CoveredBy("(4!!) (!!C)"),
    "!!4 CX"       -> MergeableCase(F0)(Nil)(R0, C1)(R0.rstate ++ C1.rstate),
    "(!!4) CX"     -> ConflictingCase(F0, R0)(C0)(Nil)(R0.rstate)(C0.rstate),
    "(!!4) CX 2"   -> MergeableCase(F0, R0)(Nil)(C1)(R0.rstate ++ C1.rstate),
    "!!4 C!"       -> MergeableCase(F0)(S1)(R0, C1)(R0.rstate ++ C1.rstate),
    "(!!4) C!"     -> CurrentConflictMergeableCase(F0, R0)(S1, C1)(Nil)(R0.rstate)(C1.rstate),
    "!!4 (C!)"     -> MergeableCase(F0)(S1, C1)(R0)(R0.rstate ++ C1.rstate),
    "(!!4) (C!)"   -> CurrentConflictMergeableCase(F0, R0)(S1, C1)(Nil)(R0.rstate)(C1.rstate),
    "(!!4) (C!) 2" -> ConflictingCase(F0, R0)(S1, C_)(Nil)(R0.rstate)(C_.rstate),
//    "!!4 C!!"      -> InfiniteLoop(F0)(R1)(R0, C1)(R0.rstate ++ C1.rstate),
    "(!!4) C!!"   -> CoveredBy("(4!!) C!!"),
    "!!4 (C!!)"   -> CoveredBy("!!4 (!!C)"),
    "(!!4) (C!!)" -> CoveredBy("(4!!) (!!C)"),
//    "!!C !!C"      -> InfiniteLoop(C1)(C0)(R0, R1)(R0.rstate ++ R1.rstate),
//    "(!!C) !!C"    -> InfiniteLoop(C1, R1)(C0)(R0)(R0.rstate),
//    "(!!C) (!!C)"  -> InfiniteLoop(C1, R1)(C0, R0)(Nil)(emptyState),
//    "!!C CX"       -> InfiniteLoop(C0)(C1)(R0)(R0.rstate ++ C1.rstate),
//    "(!!C) CX"     -> InfiniteLoop(C0, R0)(C1)(Nil)(C1.rstate),
//    "!!C C!"       -> InfiniteLoop(C0)(S1)(R0, C1)(R0.rstate ++ C1.rstate),
//    "(!!C) C!"     -> InfiniteLoop(C0, R0)(S1)(C1)(C1.rstate),
//    "!!C (C!)"     -> InfiniteLoop(C0)(S1, C1)(R0)(R0.rstate ++ C1.rstate),
//    "(!!C) (C!)"   -> InfiniteLoop(C0, R0)(S1, C1)(Nil)(C1.rstate),
//    "!!C C!!"      -> InfiniteLoop(C0)(R1)(R0, C1)(R0.rstate ++ C1.rstate),
//    "(!!C) C!!"    -> InfiniteLoop(C0, R0)(R1)(C1)(C1.rstate),
    "(!!C) (C!!)" -> CoveredBy("(!!C) (!!C)"),
    "CX CX"       -> MergeableCase(C0)(C1)(Nil)(C0.rstate ++ C1.rstate),
    "CX C!"       -> MergeableCase(S1)(S0)(C0)(S1.rstate ++ C0.rstate),
    "CX (C!)"     -> MergeableCase(C_)(C_, S0)(Nil)(C_.rstate ++ C_.rstate),
//    "CX C!!"       -> InfiniteLoop(C0)(R1)(C1)(C0.rstate ++ C1.rstate),
//    "CX (C!!)"     -> InfiniteLoop(C0)(R1, C1)(Nil)(C0.rstate),
    "C! C!"     -> MergeableCase(S1)(S0)(C_)(C_.rstate),
    "C! C! 2"   -> MergeableCase(S0)(S1)(C0, C1)(C0.rstate ++ C1.rstate),
    "(C!) C!"   -> MergeableCase(C0, S0)(S0)(C_)(C_.rstate ++ C0.rstate),
    "C! (C!)"   -> CoveredBy("(C!) C!"), // ?
    "(C!) (C!)" -> MergeableCase(C_, S0)(C_, S0)(Nil)(C_.rstate ++ C_.rstate),
//    "C! C!!"       -> InfiniteLoop(S1)(R0)(C1, C0)(C1.rstate ++ C0.rstate),
    "(C!) C!!" -> CoveredBy("(!C) C!!"),
//    "C! (C!!)"     -> InfiniteLoop(S1)(R0, C0)(C1)(C1.rstate),
    "(C!) (C!!)" -> CoveredBy("(!C) (C!!)"),
//    "C!! C!!"      -> InfiniteLoop(R0)(R1)(C1, C0)(C1.rstate ++ C0.rstate),
    "(C!!) C!!"   -> CoveredBy("(!!C) C!!"),
    "(C!!) (C!!)" -> CoveredBy("(!!C) (C!!)")
  )

  val peekMergeabilityCases = List(
    //"PX !X"    -> ConflictingCase(S0)(P_)(Nil)(S0.rstate)(P_.rstate), -> non deteministic, depends on which exactly produce is commed
    "PX !4"    -> MergeableCase(P1)(F_)(S0)(P1.rstate),
    "PX (!4)"  -> MergeableCase(P1)(F_, S0)(Nil)(P1.rstate),
    "PX !C"    -> MergeableCase(P1)(C_)(S0)(P1.rstate ++ C_.rstate),
    "PX (!C)"  -> MergeableCase(P1)(C_, S0)(Nil)(P1.rstate ++ C_.rstate),
    "PX 4X"    -> MergeableCase(F_)(P_)(Nil)(F_.rstate ++ P_.rstate),
    "PX 4!"    -> MergeableCase(F_)(P1)(S0)(P1.rstate),
    "PX (4!)"  -> CoveredBy("PX (!4)"),
    "PX 4!!"   -> CurrentConflictMergeableCase(P0)(R1)(F1)(P0.rstate ++ F1.rstate)(R1.rstate),
    "PX (4!!)" -> CurrentConflictMergeableCase(P0)(R1, F1)(Nil)(P0.rstate)(R1.rstate),
    "PX PX"    -> MergeableCase(P_)(P_)(Nil)(P_.rstate ++ P_.rstate),
    "PX P!"    -> CurrentConflictMergeableCase(P0)(S1)(P_)(P0.rstate ++ P_.rstate)(S1.rstate),
    "PX (P!)"  -> ConflictingCase(P_)(P_, S0)(Nil)(P_.rstate)(S0.rstate),
    "PX !P"    -> MergeableCase(P0)(P_)(S1)(P0.rstate ++ S1.rstate),
    "PX (!P)"  -> CoveredBy("PX (P!)"),
    "PX P!!"   -> CurrentConflictMergeableCase(P1)(R0)(P_)(P1.rstate ++ P_.rstate)(R0.rstate),
    "PX (P!!)" -> ConflictingCase(P_)(P_, R0)(Nil)(P_.rstate)(R0.rstate),
    "PX !!P"   -> MergeableCase(P1)(P_)(R0)(P1.rstate ++ R0.rstate),
    "PX (P!!)" -> CoveredBy("PX (P!!)"),
    "PX !!X "  -> CurrentConflictMergeableCase(P0)(R1)(Nil)(P0.rstate)(R1.rstate),
    "PX !!X "  -> ConflictingCase(P_)(R1)(Nil)(P_.rstate)(R1.rstate),
    "PX !!4"   -> MergeableCase(Nil)(F_)(P0, R1)(P0.rstate ++ R1.rstate),
    "PX (!!4)" -> CurrentConflictMergeableCase(P0)(F_, R1)(Nil)(P0.rstate)(R1.rstate),
//    "PX !!C"   -> InfiniteLoop(P1)(C0)(R0)(P1.rstate ++ R0.rstate),
//    "PX (!!C)" -> InfiniteLoop(P1)(C0, R0)(Nil)(P1.rstate),
    "PX CX"   -> MergeableCase(P_)(C_)(Nil)(P_.rstate ++ C_.rstate),
    "PX C!"   -> MergeableCase(P0)(C_)(S1)(P0.rstate ++ C_.rstate),
    "PX (C!)" -> MergeableCase(P_)(C_, S0)(Nil)(P_.rstate ++ C_.rstate),
//    "PX C!!"   -> InfiniteLoop(P1)(R0)(C0)(P1.rstate ++ C0.rstate),
    "PX C!!"  -> CoveredBy("PX (!!C)"),
    "P! !X"   -> MergeableCase(S0)(P_)(S0)(S0.rstate ++ S0.rstate),
    "(P!) !X" -> MergeableCase(S0)(P_, S0)(Nil)(S0.rstate ++ S0.rstate),
    "!P !X"   -> MergeableCase(S0)(S1)(P1)(S0.rstate ++ S1.rstate),
    "(!P) !X" -> CoveredBy("(P!) !X"),
    "P! !4"   -> MergeableCase(S1)(F0)(S0, P1)(S1.rstate),
//    "(P!) !4"  -> NonDeterminedCase(P_, S1)(F0)(S0)(emptyState)(emptyState),
    "(P!) !4 2" -> MergeableCase(P1, S1)(F0)(S0)(S1.rstate),
//    "P! (!4)"   -> NonDeterminedUnknownCase(S0, F0)(S1)(P_)(emptyState)(emptyState),
    "P! (!4) 2" -> MergeableCase(S0, F0)(S1)(P1)(S1.rstate),
    "(P!) (!4)" -> MergeableCase(S0, F_)(P_, S0)(Nil)(S0.rstate),
    "!P !4"     -> MergeableCase(F0)(P1)(S0, S1)(S1.rstate),
//    "!P !4 2"   -> ConflictingCase(F_)(P1)(S1)(emptyState)(S1.rstate), TODO not clear
    "(!P) !4"   -> CoveredBy("(P!) !4"),
    "!P (!4)"   -> MergeableCase(F0)(P1)(S0, S1)(S1.rstate),
    "(!P) (!4)" -> CoveredBy("(P!) (!4)"),
    "P! !C" -> CurrentConflictMergeableCase(S0)(C1)(P0, S1)(S0.rstate ++ S1.rstate)(
      C1.rstate ++ P0.rstate
    ),
//    "P! (!C)" -> NonDeterminedConflictCase(S0, C0)(S1)(P_)(emptyState)(emptyState),
    "(P!) !C"   -> ConflictingCase(C0)(P_, S1)(S0)(C0.rstate)(S0.rstate ++ S1.rstate),
    "(P!) (!C)" -> ConflictingCase(S0, C_)(P_, S0)(Nil)(C_.rstate)(S0.rstate),
    "!P !C"     -> MergeableCase(P1)(C0)(S0, S1)(C0.rstate ++ S1.rstate),
//    "!P !C 2"   -> NonDeterminedConflictCase(P_)(C_)(S0, S1)(emptyState)(emptyState),
    "(!P) !C"   -> CoveredBy("(P!) !C"),
    "!P (!C)"   -> CoveredBy("P! (!C)"),
    "(!P) (!C)" -> CoveredBy("(P!) (!C)"),
    "P! 4X"     -> MergeableCase(S1)(Nil)(P_, F0)(S1.rstate ++ F0.rstate),
    "(P!) 4X"   -> ConflictingCase(P_, S0)(F_)(Nil)(S0.rstate)(F_.rstate),
    "!P 4X"     -> MergeableCase(P_)(F0)(S1)(S1.rstate ++ F0.rstate),
    "(!P) 4X"   -> CoveredBy("(P!) 4X"),
//    "P! 4!"     -> NonDeterminedConflictCase(S1)(S0)(F_, P_)(emptyState)(emptyState),
    "P! 4!"   -> MergeableCase(S1)(S0)(P0, F1)(S0.rstate),
    "(P!) 4!" -> MergeableCase(S0)(P1, S1)(F0)(S1.rstate),
//    "(P!) 4! 2" -> NonDeterminedConflictCase(S0)(P1, S1)(F_)(emptyState)(emptyState),
    "P! (4!)" -> MergeableCase(S1)(F0, S0)(P1)(S1.rstate),
//    "P! (4!) 2" -> NonDeterminedConflictCase(S1)(F0, S0)(P_)(emptyState)(emptyState),
    "(P!) (4!)" -> MergeableCase(F_, S0)(P_, S0)(Nil)(S0.rstate),
    "!P 4!"     -> MergeableCase(P_)(S0)(F0, S1)(S1.rstate),
    "(!P) 4!"   -> CoveredBy("(P!) 4!"),
    "!P (4!)"   -> MergeableCase(P1)(F0, S0)(S1)(S1.rstate),
    "(!P) (4!)" -> CoveredBy("(P!) (4!)"),
    "P! 4!!"    -> MergeableCase(S0)(R1)(P0, F1)(S0.rstate ++ R1.rstate),
//    "P! 4!!"   -> NonDeterminedConflictCase(S0)(R1)(P0, F_)(S0.rstate ++ F_.rstate)(P0.rstate ++ R1.rstate),
//    "(P!) 4!!" -> NonDeterminedConflictCase(S1, P1)(R1)(F1)(emptyState)(emptyState),
    "(P!) 4!!"     -> MergeableCase(S0, P0)(R1)(F1)(S0.rstate ++ R1.rstate),
    "P! (4!!)"     -> MergeableCase(S0)(R1, F1)(P0)(S0.rstate ++ R1.rstate),
    "(P!) (4!!)"   -> MergeableCase(S1, P1)(R1, F1)(Nil)(S1.rstate ++ R1.rstate),
    "(P!) (4!!) 2" -> MergeableCase(S1, P1)(R0, F0)(Nil)(S1.rstate ++ R0.rstate),
    "(P!) (4!!) 3" -> MergeableCase(S1, P_)(R1, F_)(Nil)(S1.rstate ++ R1.rstate),
    "!P 4!!"       -> MergeableCase(P0)(R1)(S0, F1)(S0.rstate ++ R1.rstate),
    "!P 4!! 2"     -> MergeableCase(P_)(R1)(S0, F1)(S0.rstate ++ R1.rstate),
    "(!P) 4!!"     -> CoveredBy("(P!) 4!!"),
    "!P (4!!)"     -> MergeableCase(P0)(R1, F1)(S0)(S0.rstate ++ R1.rstate),
    "(!P) (4!!)"   -> CoveredBy("(P!) (4!!)"),
    "P! PX"        -> MergeableCase(S0)(Nil)(P1, P0)(S0.rstate ++ P1.rstate),
    "(P!) PX"      -> MergeableCase(S0, P_)(S0)(Nil)(S0.rstate ++ S0.rstate),
    "!P PX"        -> MergeableCase(P_)(Nil)(S1, P0)(S1.rstate ++ P0.rstate),
    "(!P) PX"      -> CoveredBy("(P!) PX"),
    "P! P!"        -> MergeableCase(S0)(S1)(P0, P1)(S0.rstate ++ S1.rstate),
    "P! P! 2"      -> ConflictingCase(S1)(S0)(P_)(S1.rstate)(S0.rstate),
//    "P! (P!)"    -> NonDeterminedUnknownCase(S0)(P1, S1)(P_)(emptyState)(emptyState),
    "P! (P!) 2"  -> MergeableCase(S0)(P1, S1)(P0)(S0.rstate ++ S1.rstate),
    "(P!) P!"    -> CoveredBy("P! (P!)"),
    "(P!) (P!)"  -> MergeableCase(P_, S0)(P_, S0)(Nil)(S0.rstate ++ S0.rstate),
    "P! !P"      -> MergeableCase(S0)(P1)(P0, S1)(S0.rstate ++ S1.rstate),
    "P! (!P)"    -> CoveredBy("P! (P!)"),
    "(P!) !P"    -> MergeableCase(P_)(P0, S0)(S1)(S0.rstate ++ S1.rstate),
    "(P!) (!P)"  -> CoveredBy("(P!) (P!)"),
    "!P !P"      -> MergeableCase(P0)(P1)(S0, S1)(S0.rstate ++ S1.rstate),
    "!P (!P)"    -> CoveredBy("P! (!P)"),
    "(!P) !P"    -> CoveredBy("!P (!P)"),
    "(!P) (!P)"  -> CoveredBy("(P!) (P!)"),
    "P! P!!"     -> MergeableCase(S0)(R1)(P0, P1)(S0.rstate ++ R1.rstate),
    "(P!) P!!"   -> MergeableCase(S0, P_)(R1)(P1)(S0.rstate ++ R1.rstate),
    "P! (P!!)"   -> MergeableCase(S0)(R1, P1)(P0)(S0.rstate ++ R1.rstate),
    "P! (P!!)"   -> ConflictingCase(S0)(R1, S1)(P_)(S0.rstate)(R1.rstate ++ S1.rstate),
    "(P!) (P!!)" -> MergeableCase(P_, S0)(R1, P_)(Nil)(S0.rstate ++ R1.rstate),
    "!P P!!"     -> MergeableCase(P1)(R0)(S1, P0)(S1.rstate ++ R0.rstate),
    "(!P) P!!"   -> CoveredBy("(P!) P!!"),
    "!P (P!!)"   -> MergeableCase(P_)(P_, R0)(S1)(R0.rstate ++ S1.rstate),
    "(!P) (P!!)" -> CoveredBy("(P!) (P!!)"),
    "!P !!P"     -> MergeableCase(P0)(P1)(S0, R1)(S0.rstate ++ R1.rstate),
    "(!P) !!P"   -> MergeableCase(P_, S0)(P_)(R0)(S0.rstate ++ R0.rstate),
    "!P (!!P)"   -> CoveredBy("!P (P!!)"),
    "(!P) (!!P)" -> CoveredBy("(P!) (P!!)"),
    "P! !!X"     -> MergeableCase(S1)(R0)(P0)(R0.rstate ++ S1.rstate),
    "(P!) !!X"   -> MergeableCase(S1, P_)(R0)(Nil)(S1.rstate ++ R0.rstate),
    "!P !!X"     -> MergeableCase(P_)(R1)(S0)(R1.rstate ++ S0.rstate),
    "(!P) !!X"   -> CoveredBy("(P!) !!X"),
    "P! !!4"     -> MergeableCase(S0)(F_)(P0, R1)(R1.rstate ++ S0.rstate),
    "(P!) !!4"   -> MergeableCase(S0, P0)(F_)(R1)(R1.rstate ++ S0.rstate),
    "P! (!!4)"   -> MergeableCase(S0)(F_, R1)(P0)(R1.rstate ++ S0.rstate),
    "P! (!!4) 2" -> ConflictingCase(S0)(F_, R1)(P_)(S0.rstate)(R1.rstate),
    "(P!) (!!4)" -> CoveredBy("(P!) (4!!)"),
//    "!P !!4"     -> NonDeterminedUnknownCase(P0)(F_)(S0, R1)(emptyState)(emptyState),
    "!P !!4 2"   -> MergeableCase(P0)(F1)(S0, R1)(S0.rstate ++ R1.rstate),
    "(!P) !!4"   -> CoveredBy("(!P) !!4"),
    "!P (!!4)"   -> MergeableCase(P0)(F1, R1)(S0)(S0.rstate ++ R1.rstate),
    "(!P) (!!4)" -> CoveredBy("(P!) (4!!)"),
//    "P! !!C"     -> InfiniteLoop(S1)(C0)(P1, R0)(S1.rstate ++ R0.rstate),
//    "(P!) !!C"   -> InfiniteLoop(S1, P1)(C0)(R0)(S1.rstate ++ R0.rstate),
//    "P! (!!C)"   -> InfiniteLoop(S1)(C0, R0)(P1)(S1.rstate),
//    "(P!) (!!C)" -> InfiniteLoop(S1, P1)(C0, R0)(Nil)(S1.rstate),
    "P! CX"   -> CurrentConflictMergeableCase(S0)(C1)(P_)(S0.rstate)(C1.rstate ++ P_.rstate),
    "P! CX 2" -> ConflictingCase(S0)(C_)(P_)(S0.rstate)(C_.rstate ++ P_.rstate),
    "(P!) CX" -> ConflictingCase(C_)(P_, S0)(Nil)(C_.rstate)(S0.rstate),
    "!P CX"   -> MergeableCase(P_)(C1)(S0)(S0.rstate ++ C1.rstate),
    "(!P) CX" -> CoveredBy("(P!) CX"),
//    "P! C!"      -> NonDeterminedUnknownCase(S0)(S0)(P_, C_)(emptyState)(emptyState),
    "P! C! 2" -> MergeableCase(S1)(S0)(P1, C0)(S1.rstate ++ C0.rstate),
    "P! (C!)" -> CurrentConflictMergeableCase(S1)(C0, S0)(P1)(S1.rstate)(C0.rstate ++ P1.rstate),
    "(P!) C!" -> MergeableCase(S1)(P0, S0)(C1)(S0.rstate ++ C1.rstate),
//    "(P!) C! 2"  -> NonDeterminedUnknownCase(S1)(P0, S0)(C_)(emptyState)(emptyState),
    "(P!) (C!)" -> ConflictingCase(P_, S0)(C_, S0)(Nil)(S0.rstate)(C_.rstate),
    "!P C!"     -> MergeableCase(P_)(S1)(S0, C1)(S0.rstate ++ C1.rstate),
    "!P (C!)"   -> MergeableCase(P_)(C0, S0)(S1)(C0.rstate ++ S1.rstate),
    "(!P) C!"   -> CoveredBy("(P!) C!"),
    "(!P) (C!)" -> CoveredBy("(P!) (C!)"),
//    "P! C!!"     -> InfiniteLoop(S1)(R0)(P1, C0)(S1.rstate ++ C0.rstate),
//    "(P!) C!!"   -> InfiniteLoop(S1, P1)(R0)(C0)(S1.rstate ++ C0.rstate),
    "P! (C!!)"   -> CoveredBy("P! (!!C)"),
    "(P!) (C!!)" -> CoveredBy("(P!) (!!C)"),
    "P!! !X"     -> MergeableCase(Nil)(R0)(P0, S1)(R0.rstate ++ S1.rstate),
    "(P!!) !X "  -> MergeableCase(S0)(P_, R0)(Nil)(S0.rstate ++ R0.rstate),
    "!!P !X"     -> MergeableCase(Nil)(P0)(R0, S1)(R0.rstate ++ S1.rstate),
    "(!!P) !X"   -> CoveredBy("!!P !X"),
    "P!! !4"     -> MergeableCase(F0)(R1)(S0, P1)(R1.rstate),
//    "(P!!) !4"   -> NonDeterminedUnknownCase(F1)(P_, R0)(S1)(emptyState)(emptyState),
    "P!! (!4)"   -> MergeableCase(S0, F0)(R1)(P1)(R1.rstate),
    "(P!!) (!4)" -> MergeableCase(S0, F0)(P_, R0)(Nil)(R0.rstate),
//    "!!P !4"     -> NonDeterminedUnknownCase(F0)(P_)(S0, R1)(emptyState)(emptyState),
    "!!P !4 2"   -> MergeableCase(F0)(P1)(S0, R1)(R1.rstate),
    "(!!P) !4"   -> CoveredBy("(P!!) !4"),
    "!!P (!4)"   -> MergeableCase(S0, F0)(P_)(R1)(R1.rstate),
    "(!!P) (!4)" -> CoveredBy("(P!!) (!4)"),
    "P!! !C"     -> MergeableCase(C0)(P1)(S0, R1)(C0.rstate ++ R1.rstate),
    "(P!!) !C"   -> CurrentConflictMergeableCase(C0)(R1, P1)(S0)(C0.rstate)(R1.rstate ++ S0.rstate),
//    "(P!!) !C 2" -> NonDeterminedUnknownCase(C_)(R1, P_)(S0)(emptyState)(emptyState),
    "P!! (!C)"   -> MergeableCase(S0, C0)(P_)(R1)(R1.rstate ++ C0.rstate),
    "(P!!) (!C)" -> ConflictingCase(S0, C_)(P_, R0)(Nil)(C_.rstate)(R0.rstate),
    "!!P !C"     -> MergeableCase(C0)(P1)(S0, R1)(C0.rstate ++ R1.rstate),
    "(!!P) !C"   -> CoveredBy("(P!!) !C"),
    "!!P (!C)"   -> MergeableCase(S0, C0)(P_)(R1)(R1.rstate ++ C0.rstate),
    "(!!P) (!C)" -> CoveredBy("(P!!) (!C)"),
    "P!! 4X"     -> CurrentConflictMergeableCase(F1)(R0)(P_)(F1.rstate ++ P_.rstate)(R0.rstate),
    "P!! 4X 2"   -> ConflictingCase(F_)(R0)(P_)(F_.rstate ++ P_.rstate)(R0.rstate),
    "(P!!) 4X"   -> ConflictingCase(F_)(P_, R0)(Nil)(F_.rstate)(R0.rstate),
    "!!P 4X"     -> MergeableCase(F_)(P_)(R0)(R0.rstate),
    "(!!P) 4X"   -> CoveredBy("(P!!) 4X"),
    "P!! 4!"     -> MergeableCase(R1)(S0)(P1, F0)(R1.rstate),
    "(P!!) 4!"   -> MergeableCase(S0)(P1, R1)(F0)(R1.rstate),
    "P!! (4!)"   -> MergeableCase(F0, S0)(P_)(R1)(R1.rstate),
    "(P!!) (4!)" -> MergeableCase(F_, S0)(P_, R0)(Nil)(R0.rstate),
    "!!P 4!"     -> MergeableCase(F0)(P1)(S0, R1)(R1.rstate),
    "(!!P) 4!"   -> CoveredBy("(P!!) 4!"),
//    "!!P (4!)"   -> NonDeterminedMergeableCase(F_, S0)(P_)(R1)(R1.rstate)(R1.rstate),
    "(!!P) (4!)"  -> CoveredBy("(P!!) (4!)"),
    "P!! 4!!"     -> MergeableCase(R0)(R1)(P0, F1)(R1.rstate ++ R0.rstate),
    "P!! 4!! 2"   -> ConflictingCase(R0)(R1)(P_, F_)(R0.rstate)(R1.rstate),
    "(P!!) 4!!"   -> MergeableCase(P0, R0)(R1)(F1)(R0.rstate ++ R1.rstate),
    "P!! (4!!)"   -> MergeableCase(R0)(R1, F1)(P0)(R0.rstate ++ R1.rstate),
    "(P!!) (4!!)" -> MergeableCase(R0, P_)(R1, F_)(Nil)(R0.rstate ++ R1.rstate),
    "!!P 4!!"     -> MergeableCase(P_)(R0)(R1, F1)(R0.rstate ++ R1.rstate),
    "(!!P) 4!!"   -> CoveredBy("(P!!) 4!!"),
    "!!P (4!!)"   -> MergeableCase(P_)(F_, R1)(R0)(R0.rstate ++ R1.rstate),
    "(!!P) (4!!)" -> CoveredBy("(P!!) (4!!)"),
    "P!! PX"      -> MergeableCase(R0)(S1)(P0)(S1.rstate ++ R0.rstate),
    "(P!!) PX"    -> CurrentConflictMergeableCase(P_, R0)(P1)(Nil)(R0.rstate)(P1.rstate),
    "(P!!) PX 2"  -> ConflictingCase(P_, R0)(P_)(Nil)(R0.rstate)(P_.rstate),
    "!!P PX"      -> MergeableCase(P_)(P1)(R0)(R0.rstate ++ P1.rstate),
    "(P!!) PX"    -> CoveredBy("(P!!) PX"),
    "P!! P!"      -> CoveredBy("P! P!!"),
    "(P!!) P!"    -> CoveredBy("P! (P!!)"),
    "P!! (P!)"    -> CoveredBy("(P!) P!!"),
    "(P!!) (P!)"  -> CoveredBy("(P!) (P!!)"),
    "!!P P!"      -> MergeableCase(S0)(P1)(P0, R1)(R1.rstate ++ S0.rstate),
    "(!!P) P!"    -> CoveredBy("(P!!) P!"),
//    "!!P (P!)"    -> NonDeterminedMergeableCase(P_)(P_, S0)(R0)(R0.rstate)(R0.rstate),
    "(!!P) (P!)"  -> CoveredBy("(P!!) (P!)"),
    "P!! P!!"     -> MergeableCase(R1)(R0)(P1, P0)(R0.rstate ++ R1.rstate),
    "(P!!) P!!"   -> MergeableCase(R0)(P_, R1)(P0)(R0.rstate ++ R1.rstate),
    "(P!!) (P!!)" -> MergeableCase(P_, R0)(P_, R0)(Nil)(R0.rstate ++ R0.rstate),
    "!!P P!!"     -> MergeableCase(R1)(P0)(R0, P1)(R0.rstate ++ R1.rstate),
    "(!!P) P!!"   -> CoveredBy("(P!!) P!!"),
    "(!!P) (P!!)" -> CoveredBy("(P!!) (P!!)"),
    "P!! !!X"     -> MergeableCase(R1)(R0)(P1)(R0.rstate ++ R1.rstate),
    "(P!!) !!X"   -> MergeableCase(R1, P1)(R0)(Nil)(R0.rstate ++ R1.rstate),
    "!!P !!X"     -> MergeableCase(P_)(Nil)(R1, R0)(R0.rstate ++ R1.rstate),
    "P!! !!4"     -> MergeableCase(R0)(F1)(P0, R1)(R0.rstate ++ R1.rstate),
    "P!! !!4"     -> MergeableCase(R0)(F_)(P0, R1)(R0.rstate ++ R1.rstate),
    "(P!!) !!4"   -> MergeableCase(R0, P_)(F_)(R1)(R0.rstate ++ R1.rstate),
    "P!! (!!4)"   -> CoveredBy("P!! (4!!)"),
    "(P!!) (!!4)" -> CoveredBy("(P!!) (4!!)"),
    "!!P !!4"     -> MergeableCase(P_)(F_)(R0)(R0.rstate),
    "(!!P) !!4"   -> CoveredBy("(P!!) !!4"),
    "!!P (!!4)"   -> CoveredBy("!!P (4!!)"),
    "(P!!) (!!4)" -> CoveredBy("(P!!) (4!!)"),
//    "P!! !!C"     -> InfiniteLoop(R0)(C1)(P0, R1)(R0.rstate ++ R1.rstate),
//    "(P!!) !!C"   -> InfiniteLoop(R0, P0)(C1)(R1)(R0.rstate ++ R1.rstate),
//    "P!! (!!C)"   -> InfiniteLoop(R0)(C1, R1)(P0)(R0.rstate),
//    "(P!!) (!!C)" -> InfiniteLoop(R0, P0)(C1, R1)(Nil)(R0.rstate),
    "P!! CX"   -> CurrentConflictMergeableCase(R0)(C1)(P_)(R0.rstate)(C1.rstate ++ P_.rstate),
    "P!! CX"   -> ConflictingCase(R0)(C_)(P_)(R0.rstate)(C_.rstate ++ P_.rstate),
    "(P!!) CX" -> ConflictingCase(C_)(P_, R0)(Nil)(C_.rstate)(R0.rstate),
    "!!P CX"   -> MergeableCase(P_)(C1)(R0)(R0.rstate ++ C1.rstate),
    "(!!P) CX" -> CoveredBy("(P!!) CX"),
    "P!! C!"   -> MergeableCase(S1)(R0)(C1, P0)(R0.rstate ++ C1.rstate),
    "(P!!) C!" -> MergeableCase(S1)(P_, R0)(C1)(R0.rstate ++ C1.rstate),
//    "P!! (C!)"     -> NonDeterminedConflictCase(R0)(C_, S0)(P_)(R0.rstate)(C_.rstate),
//    "P!! (C!) 2"   -> NonDeterminedConflictCase(R0)(C1, S1)(P_)(R0.rstate)(C1.rstate),
    "(P!!) (C!)"   -> ConflictingCase(P_, R0)(C_, S0)(Nil)(R0.rstate)(C_.rstate),
    "(P!!) (C!) 2" -> CurrentConflictMergeableCase(P1, R1)(C0, S0)(Nil)(R1.rstate)(C0.rstate),
    "!!P C!"       -> MergeableCase(P0)(S1)(R0, C1)(R0.rstate ++ C1.rstate),
    "(!!P) C!"     -> CoveredBy("(P!!) C!"),
    "!!P (C!)"     -> CoveredBy("!!P (!C)"),
    "(!!P) (C!)"   -> CoveredBy("(P!!) (C!)"),
//    "P!! C!!"      -> InfiniteLoop(R1)(R0)(P1, C0)(R1.rstate ++ C0.rstate),
//    "(P!!) C!!"    -> InfiniteLoop(R1, P1)(R0)(C0)(R1.rstate ++ C0.rstate),
    "P!! (C!!)"   -> CoveredBy("P!! (!!C)"),
    "(P!!) (C!!)" -> CoveredBy("(P!!) (C!!)")
  )

  val joinMergeabilityCases = List(
    "J S S"   -> ConflictingCase(S0)(S1)(J_)(J_.rstate ++ S0.rstate)(J_.rstate ++ S1.rstate),
    "J S N"   -> MergeableCase(S0)(Nil)(J_)(J_.rstate ++ S0.rstate),
    "J S 4"   -> ConflictingCase(S0)(F_)(J_)(J_.rstate ++ S0.rstate)(J_.rstate ++ F_.rstate),
    "J S C"   -> ConflictingCase(S0)(C_)(J_)(J_.rstate ++ S0.rstate)(J_.rstate ++ C_.rstate),
    "J S R"   -> ConflictingCase(S0)(R1)(J_)(J_.rstate ++ S0.rstate)(J_.rstate ++ R1.rstate),
    "J S P"   -> ConflictingCase(S0)(P_)(J_)(J_.rstate ++ S0.rstate)(J_.rstate ++ P_.rstate),
    "J 4 4"   -> MergeableCase(F_)(F1)(J_)(J_.rstate ++ F_.rstate ++ F1.rstate),
    "J 4 N"   -> MergeableCase(F_)(Nil)(J_)(J_.rstate ++ F_.rstate),
    "J 4 C"   -> MergeableCase(F_)(C_)(J_)(J_.rstate ++ F_.rstate ++ C_.rstate),
    "J 4 R"   -> ConflictingCase(F_)(R1)(J_)(J_.rstate ++ F_.rstate)(J_.rstate ++ R1.rstate),
    "J 4 P"   -> MergeableCase(F_)(P_)(J_)(J_.rstate ++ F_.rstate ++ P_.rstate),
    "J C C"   -> MergeableCase(C_)(C1)(J_)(J_.rstate ++ C_.rstate ++ C1.rstate),
    "J C R"   -> ConflictingCase(C_)(R1)(J_)(J_.rstate ++ C_.rstate)(J_.rstate ++ R1.rstate),
    "J C P"   -> MergeableCase(C_)(P_)(J_)(J_.rstate ++ C_.rstate ++ P_.rstate),
    "J C N"   -> MergeableCase(C_)(Nil)(J_)(J_.rstate ++ C_.rstate),
    "J R R"   -> ConflictingCase(R0)(R1)(J_)(J_.rstate ++ R0.rstate)(J_.rstate ++ R1.rstate),
    "J R P"   -> ConflictingCase(R0)(P_)(J_)(J_.rstate ++ R0.rstate)(J_.rstate ++ P_.rstate),
    "J R N"   -> MergeableCase(R0)(Nil)(J_)(J_.rstate ++ R0.rstate),
    "J P P"   -> MergeableCase(P1)(P0)(J_)(J_.rstate ++ P1.rstate ++ P0.rstate),
    "J P N"   -> MergeableCase(P1)(Nil)(J_)(J_.rstate ++ P1.rstate),
    "J N N"   -> MergeableCase(Nil)(Nil)(J_)(J_.rstate),
    "S J J"   -> MergeableCase(J_)(J_)(S0)(J_.rstate ++ J_.rstate ++ S0.rstate),
    "S J S"   -> ConflictingCase(S0)(J_)(S0)(S0.rstate ++ S0.rstate)(J_.rstate ++ S0.rstate),
    "S J 4"   -> ConflictingCase(F0)(J_)(S0)(emptyState)(J_.rstate ++ S0.rstate),
    "S J 4 2" -> MergeableCase(F1)(J_)(S0)(J_.rstate ++ F1.rstate ++ S0.rstate),
    "S J C"   -> ConflictingCase(C0)(J_)(S0)(C0.rstate)(J_.rstate ++ S0.rstate),
    "S J C 2" -> MergeableCase(C1)(J_)(S0)(J_.rstate ++ C1.rstate ++ S0.rstate),
    "S J R"   -> ConflictingCase(R0)(J_)(S0)(R0.rstate ++ S0.rstate)(S0.rstate ++ J_.rstate),
    "S J P"   -> ConflictingCase(P_)(J_)(S0)(S0.rstate)(J_.rstate ++ S0.rstate),
    "S J P 2" -> MergeableCase(P1)(J_)(S0)(J_.rstate ++ S0.rstate ++ P1.rstate),
    "S J N"   -> MergeableCase(Nil)(J_)(S0)(J_.rstate ++ S0.rstate),
    "4 J J"   -> MergeableCase(J_)(J_)(F_)(J_.rstate ++ J_.rstate ++ F_.rstate),
    "4 J S"   -> ConflictingCase(J_)(S1)(F_)(J_.rstate ++ F_.rstate)(emptyState),
    "4 J 4"   -> MergeableCase(J_)(F1)(F_)(J_.rstate ++ F_.rstate ++ F1.rstate),
    "4 J C"   -> MergeableCase(C0)(J_)(F_)(J_.rstate ++ C0.rstate ++ F_.rstate),
    "4 J R"   -> ConflictingCase(R0)(J_)(F_)(R0.rstate)(F_.rstate ++ J_.rstate),
    "4 J R 2" -> ConflictingCase(R0)(J_)(F1)(F1.rstate ++ R0.rstate)(F1.rstate ++ J_.rstate),
    "4 J P"   -> MergeableCase(P_)(J_)(F_)(J_.rstate ++ F_.rstate ++ P_.rstate),
    "4 J N"   -> MergeableCase(Nil)(J_)(F_)(J_.rstate ++ F_.rstate),
    "C J J"   -> MergeableCase(J_)(J_)(C_)(J_.rstate ++ J_.rstate ++ C_.rstate),
    "C J S"   -> ConflictingCase(J_)(S1)(C_)(J_.rstate ++ C_.rstate)(C_.rstate),
    "C J 4"   -> MergeableCase(J_)(F1)(C_)(J_.rstate ++ C_.rstate ++ F1.rstate),
    "C J C"   -> MergeableCase(C0)(J_)(C_)(J_.rstate ++ C0.rstate ++ C_.rstate),
//    "C J R"   -> InfiniteLoop(R0)(J_)(C_)(C_.rstate ++ J_.rstate),
    "C J R 2" -> ConflictingCase(R0)(J_)(C1)(C1.rstate ++ R0.rstate)(J_.rstate ++ C1.rstate),
    "C J P"   -> MergeableCase(P_)(J_)(C_)(J_.rstate ++ C_.rstate ++ P_.rstate),
    "C J N"   -> MergeableCase(Nil)(J_)(C_)(J_.rstate ++ C_.rstate),
    "R J J"   -> MergeableCase(J_)(J_)(R0)(J_.rstate ++ R0.rstate),
    "R J S"   -> ConflictingCase(J_)(S1)(R0)(J_.rstate ++ R0.rstate)(R0.rstate ++ S1.rstate),
    "R J 4"   -> MergeableCase(J_)(F1)(R0)(J_.rstate ++ R0.rstate ++ F1.rstate),
    "R J 4 2" -> ConflictingCase(J_)(F0)(R0)(J_.rstate ++ R0.rstate)(R0.rstate),
    "R J C"   -> MergeableCase(C0)(J_)(R0)(J_.rstate ++ R0.rstate),
    "R J C 2" -> MergeableCase(C1)(J_)(R0)(J_.rstate ++ R0.rstate ++ C1.rstate),
    "R J R"   -> ConflictingCase(R0)(J_)(R0)(R0.rstate ++ R0.rstate)(R0.rstate ++ J_.rstate), //???
    "R J P"   -> ConflictingCase(P_)(J_)(R0)(R0.rstate)(R0.rstate ++ J_.rstate),
    "R J P 2" -> MergeableCase(P1)(J_)(R0)(R0.rstate ++ P1.rstate ++ J_.rstate),
    "R J N"   -> MergeableCase(Nil)(J_)(R0)(J_.rstate ++ R0.rstate),
    "P J J"   -> MergeableCase(J_)(J_)(P_)(J_.rstate ++ J_.rstate ++ P_.rstate),
    "P J S"   -> ConflictingCase(J_)(S1)(P_)(J_.rstate ++ P_.rstate)(S1.rstate),
    "P J S 2" -> ConflictingCase(J_)(S1)(P0)(J_.rstate ++ P0.rstate)(S1.rstate ++ P0.rstate),
    "P J 4"   -> MergeableCase(J_)(F1)(P_)(J_.rstate ++ P_.rstate ++ F1.rstate),
    "P J C"   -> MergeableCase(C0)(J_)(P_)(J_.rstate ++ C0.rstate ++ P_.rstate),
    "P J R"   -> ConflictingCase(R0)(J_)(P_)(R0.rstate)(J_.rstate ++ P_.rstate),
    "P J R 2" -> ConflictingCase(R0)(J_)(P1)(R0.rstate ++ P1.rstate)(J_.rstate ++ P1.rstate),
    "P J P"   -> MergeableCase(P1)(J_)(P_)(J_.rstate ++ P_.rstate ++ P1.rstate),
    "P J N"   -> MergeableCase(Nil)(J_)(P_)(J_.rstate ++ P_.rstate),
    "N J J"   -> MergeableCase(J_)(J_)(Nil)(J_.rstate ++ J_.rstate),
    "N J S"   -> ConflictingCase(J_)(S1)(Nil)(J_.rstate)(S1.rstate),
    "N J 4"   -> MergeableCase(J_)(F1)(Nil)(J_.rstate ++ F1.rstate),
    "N J C"   -> MergeableCase(C0)(J_)(Nil)(J_.rstate ++ C0.rstate),
    "N J R"   -> ConflictingCase(R0)(J_)(Nil)(R0.rstate)(J_.rstate),
    "N J P"   -> MergeableCase(P1)(J_)(Nil)(J_.rstate ++ P1.rstate),
    "N J N"   -> MergeableCase(Nil)(J_)(Nil)(J_.rstate)
  )
  // need to implement how to check the conflict situation
  def conflicts(
      b1: Rho,
      b2: Rho,
      base: Rho,
      b1BaseMergedState: State,
      b2BaseMergedState: State
  )(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ): Effect[Boolean]

  // need to implement how to check the merges(non-conflict) situation
  def merges(
      b1: Rho,
      b2: Rho,
      base: Rho,
      b1BaseMergedState: State,
      b2BaseMergedState: State
  )(
      implicit file: sourcecode.File,
      line: sourcecode.Line
  ): Effect[Boolean]

}
